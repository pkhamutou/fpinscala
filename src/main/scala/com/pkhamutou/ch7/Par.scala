package com.pkhamutou.ch7

import java.util.concurrent._

object Par {

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def cancel(evenIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(l: Long, timeUnit: TimeUnit): A = get
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }

  def fork[A](a: => Par[A]): Par[A] = (es: ExecutorService) => es.submit(new Callable[A] {
    def call(): A = a(es).get
  })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /* This version respects timeouts. See `Map2Future` below. */
  def map2to[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    es => {
      val (af, bf) = (a(es), b(es))
      Map2Future(af, bf, f)
    }

  /*
    Note: this implementation will not prevent repeated evaluation if multiple threads call `get` in parallel.
    We could prevent this using synchronization,
    but it isn't needed for our purposes here (also, repeated evaluation of pure values won't affect results).
  */
  case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None

    def isDone = cache.isDefined

    def isCancelled = a.isCancelled || b.isCancelled

    def cancel(evenIfRunning: Boolean) =
      a.cancel(evenIfRunning) || b.cancel(evenIfRunning)

    def get = compute(Long.MaxValue)

    def get(timeout: Long, units: TimeUnit): C =
      compute(TimeUnit.NANOSECONDS.convert(timeout, units))

    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.nanoTime
        val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime;
        val aTime = stop - start
        val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
    }
  }

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  def sortPar(xs: Par[List[Int]]): Par[List[Int]] = map(xs)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight(unit(List.empty[A])) {
    case (a, z) => map2(a, z)(_ :: _)
  }

  // This implementation forks the recursive step off to a new logical thread,
  // making it effectively tail-recursive. However, we are constructing
  // a right-nested parallel program, and we can get better performance by
  // dividing the list in half, and running both halves in parallel.
  // See `sequenceBalanced` below.
  def sequenceRight[A](as: List[Par[A]]): Par[List[A]] = as match {
    case Nil => unit(Nil)
    case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
  }

  // We define `sequenceBalanced` using `IndexedSeq`, which provides an
  // efficient function for splitting the sequence in half.
  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence2[A](as: List[Par[A]]): Par[List[A]] = map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val xs: List[Par[A]] = as.map(a => lazyUnit(a))
    xs.foldRight(unit(List.empty[A])) {
      case (a, z) => map2(a, z) {
        case (aa, zz) if f(aa) => aa :: zz
        case (_, zz) => zz
      }
    }
  }

  trait Future[+A] {
    private[parallelism] def apply(k: A => Unit): Unit
  }

  def flatMap[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
    es => new Future[B] {
      def apply(cb: B => Unit): Unit = p(es)(a => f(a)(es)(cb))
    }

}
