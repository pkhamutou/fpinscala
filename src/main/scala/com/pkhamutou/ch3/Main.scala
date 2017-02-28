package com.pkhamutou.ch3

object Main extends App {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))


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
  }

  val xs = List(1, 2, 3, 4)

  println(List.tail(xs))
  println(List.setHead(xs, 9))
  println(List.drop(xs, 4))
  println(List.dropWhile[Int](xs, x => x < 3))
  println(List.dropWhileF(xs)(x => x < 3))

}
