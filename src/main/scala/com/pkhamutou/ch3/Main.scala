package com.pkhamutou.ch3

object Main extends App {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def foldRight[A, B](xs: List[A], z: B)(f: (A, B) => B): B = xs match {
      case Nil => z
      case Cons(x, t) => f(x, foldRight(t, z)(f))
    }

    def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }


    /*
     * Exercise 3.2
     */
    def tail[A](xs: List[A]): List[A] = xs match {
      case Nil => sys.error("empty list")
      case Cons(_, t) => t
    }

    /*
     * Exercise 3.3
     */
    def setHead[A](xs: List[A], x: A): List[A] = xs match {
      case Nil => sys.error("empty list")
      case Cons(_, t) => Cons(x, t)
    }

    /*
     * Exercise 3.4
     */
    def drop[A](xs: List[A], n: Int): List[A] = xs match {
      case ys if n <= 0 => ys
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

    /*
     * Exercise 3.5
     */
    def dropWhile[A](xs: List[A], f: A => Boolean): List[A] = xs match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => xs
    }

    def dropWhileF[A](xs: List[A])(f: A => Boolean): List[A] = xs match {
      case Cons(h, t) if f(h) => dropWhileF(t)(f)
      case _ => xs
    }

    /*
     * Exercise 3.6
     */
    def init[A](xs: List[A]): List[A] = xs match {
      case Nil => sys.error("empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

    /*
     * Exercise 3.9
     */
    def length[A](xs: List[A]): Int =
      foldRight(xs, 0)((_, x) => x + 1)

    /*
     * Exercise 3.10
     */
    def foldLeft[A, B](xs: List[A], z: B)(f: (B, A) => B): B = xs match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

    /*
     * Exercise 3.11
     */
    def sum3(xs: List[Int]): Int = xs match {
      case Nil => sys.error("empty list")
      case _ => foldLeft(xs, 0)(_ + _)
    }

    def product3(xs: List[Double]): Double = xs match {
      case Nil => sys.error("empty list")
      case _ => foldLeft(xs, 1.0)(_ * _)
    }

    def length2[A](xs: List[A]): Int =
      foldLeft(xs, 0)((x, _) => x + 1)

    /*
     * Exercise 3.12
     */
    def reverse[A](xs: List[A]): List[A] = {
      def loop(xs: List[A], rev: List[A]): List[A] = xs match {
        case Nil => rev
        case Cons(h, t) => loop(t, Cons(h, rev))
      }
      loop(xs, Nil: List[A])
    }

    def reverse2[A](xs: List[A]): List[A] =
      foldLeft(xs, Nil: List[A])((acc, h) => Cons(h, acc))

  }

  val xs = List(1, 2, 3, 4)

  println(List.tail(xs))
  println(List.setHead(xs, 9))
  println(List.drop(xs, 4))
  println(List.dropWhile[Int](xs, x => x < 3))
  println(List.dropWhileF(xs)(x => x < 3))

  println(List.foldRight(xs, 0)((x, y) => x + y))
  println(List.init(xs))

  println(List.length(xs))

  println(List.foldLeft(xs, 0)((x, y) => x + y))

  println(List.sum3(xs))
  println(List.product3(List(1.0, 2.0)))
  println(List.length2(xs))

  println(List.reverse(xs))
  println(List.reverse2(xs))

}
