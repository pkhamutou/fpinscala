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

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
  }

  val stream = Stream(1, 2, 3, 4)


  println("++++")
  println(stream.append(123).toList)
  println(stream.flatMap(x => Stream(x.toString)).map(_ + " q").toList)
  println(stream.merge(stream).toList)

  val ones: Stream[Int] = Stream.constant(1)

  println(Stream.fibs.take(10).toList)

}
