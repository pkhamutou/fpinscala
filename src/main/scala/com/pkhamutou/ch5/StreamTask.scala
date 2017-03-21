package com.pkhamutou.ch5

import scala.{Stream => _}

object StreamTask extends App {

  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, _) => Some(h())
    }

    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
      case _ => Empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
      case _ => Empty
    }

    def exists(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

    def foldRight[B](z: B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def exists2(p: A => Boolean): Boolean = foldRight(false)((a, z) => p(a) || z)

    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, z) => p(a) && z)

    def takeWhile2(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A]) { (a, z) =>
      if (p(a)) Cons(() => a, () => z) else Empty
    }

    def headOption2: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

    def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B]) { (a, z) =>
      Cons(() => f(a), () => z)
    }

    def filter(f: A => Boolean): Stream[A] = foldRight(Empty: Stream[A]) { (a, z) =>
      if (f(a)) Cons(() => a, () => z) else z
    }

    def append[B >: A](element: => B): Stream[B] = foldRight(Stream(element))((a, z) => Stream.cons(a, z))

    def merge[B >: A](element: => Stream[B]): Stream[B] = foldRight(element)((a, z) => Stream.cons(a, z))

    def flatMap[B](f: A => Stream[B]): Stream[B] = map(f).foldRight(Stream.empty[B])((a, z) => a.merge(z))

    def mapUnfold[B](f: A => B): Stream[B] = Stream.unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

    def takeUnfold(n: Int): Stream[A] = Stream.unfold((this, n)) {
      case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
      case _ => None
    }

    def takeWhileUnfold(p: A => Boolean): Stream[A] = Stream.unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

    def zipWith[B, C](xs: Stream[B])(f: (A, B) => C): Stream[C] = Stream.unfold((this, xs)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

    def zip[B](xs: Stream[B]): Stream[(A, B)] = zipWith(xs)((_, _))

    def zipAll[B](xs: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold((this, xs)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (_, Cons(h2, t2)) => Some(((None, Some(h2())), (Stream.empty, t2())))
      case (Cons(h1, t1), _) => Some(((Some(h1()), None), (t1(), Stream.empty)))
      case _ => None
    }

    def startsWith[A](xs: Stream[A]): Boolean = ???

    def tails: Stream[Stream[A]] = ???
  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](xs: A*): Stream[A] = if (xs.isEmpty) empty else cons(xs.head, apply(xs.tail: _*))

    def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

    def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

    def fibs: Stream[Int] = {
      def loop(f1: Int, f2: Int): Stream[Int] =
        cons(f1, loop(f2, f1 + f2))
      loop(0, 1)
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }

    def fibsUnfold: Stream[Int] = unfold((0, 1)){ case (f1, f2) => Some((f1, (f2, f1 + f2))) }
    def fromUnfold(n: Int): Stream[Int] = unfold(n)(x => Some(x, x + 1))
    def constantUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))
    def onesUnfold: Stream[Int] = unfold(1)(_ => Some(1, 1))
  }
}
