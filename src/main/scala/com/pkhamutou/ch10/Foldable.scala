package com.pkhamutou.ch10

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(M: Monoid[B]): B
  def concatenate[A](as: F[A])(M: Monoid[A]): A = foldLeft(as)(M.zero)(M.op)

  def toList[A](as: F[A]): List[A] = foldRight(as)(List.empty[A])(_ :: _)
}

object Foldable {
  val listFoldable = new Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)
    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) = as.foldLeft(z)(f)
    override def foldMap[A, B](as: List[A])(f: (A) => B)(M: Monoid[B]) = foldLeft(as)(M.zero)((b, a) => M.op(b, f(a)))
  }

  val indexedSeqFoldable = new Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)
    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) = as.foldLeft(z)(f)
    override def foldMap[A, B](as: IndexedSeq[A])(f: (A) => B)(M: Monoid[B]) = foldLeft(as)(M.zero)((b, a) => M.op(b, f(a)))
  }

  val streamFoldable = new Foldable[Stream] {
    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)
    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) = as.foldLeft(z)(f)
    override def foldMap[A, B](as: Stream[A])(f: (A) => B)(M: Monoid[B]) = foldLeft(as)(M.zero)((b, a) => M.op(b, f(a)))
  }
}
