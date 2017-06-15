package com.pkhamutou.ch6

/**
  * Created by pkhamutou on 6/13/17.
  */
trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (r, s) = rng.nextInt
    (if (r < 0) -(r + 1) else r, s)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (r, s) = nonNegativeInt(rng)
    (r / (Int.MaxValue.toDouble + 1), s)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, s1) = rng.nextInt
    val (d, s2) = double(s1)
    ((i, d), s2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (r, s) = intDouble(rng)
    (r.swap, s)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, s1) = double(rng)
    val (d2, s2) = double(s1)
    val (d3, s3) = double(s2)
    ((d1, d2, d3), s3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(count: Int, rng: RNG, xs: List[Int]): (List[Int], RNG) = count match {
      case 0 => (xs, rng)
      case _ => 
        val (r, s) = rng.nextInt
        loop(count - 1, s, r :: xs)
    }
    loop(count, rng, Nil)
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  val int: Rand[Int] = _.nextInt

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  val doubleMap: Rand[Double] = map(nonNegativeInt)(d => d / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, sa) = ra(rng)
    val (b, sb) = rb(sa)
    (f(a, b), sb)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A]))((a, z) => map2(a, z)(_ :: _))

  def intsSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (r, s) = f(rng)
    g(r)(s)
  }

  def nonNegativeLessThen(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThen(n)
  }

  def _map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(x => unit(f(x)))

  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a, b)))

  def rollDie: Rand[Int] = _map(nonNegativeLessThen(6))(_ + 1)
}

case class State[S, +A](run: S => (A, S)) {
  import State._
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = run(s)
    f(a).run(s2)
  })

  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](xs: List[State[S, A]]): State[S, List[A]] =
    xs.foldRight(unit[S, List[A]](List.empty[A]))((a, z) => a.map2(z)(_ :: _))

  val int: Rand[Int] = State(_.nextInt)

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = get.flatMap(s => set(f(s)))

}
