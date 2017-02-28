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

    /*
     * Exercise 3.13
     */
    def foldRight2[A, B](xs: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(reverse2(xs), z)((b, a) => f(a, b))

    def foldLeft2[A, B](xs: List[A], z: B)(f: (B, A) => B): B =
      foldRight(reverse2(xs), z)((a,  b) => f(b, a))

    /*
     * Exercise 3.14
     */
    def append2[A](xs: List[A], ys: List[A]): List[A] =
      foldLeft(reverse2(xs), ys)((l, e) => Cons(e, l))

    def append3[A](xs: List[A], ys: List[A]): List[A] =
      foldRight(xs, ys)((e, l) => Cons(e, l))

    /*
     * Exercise 3.15
     */
    def concat[A](xss: List[List[A]]): List[A] =
      foldRight(xss, Nil: List[A])((xs, y) => foldRight(xs, y)((h, t) => Cons(h, t)))

    def concat2[A](xss: List[List[A]]): List[A] =
      foldRight(xss, Nil: List[A])(append3)
    /*
     * Exercise 3.16
     */
    def plusOne(xs: List[Int]): List[Int] =
      foldRight(xs, Nil: List[Int])((h, t) => Cons(h + 1, t))

    def plusOne2(xs: List[Int]): List[Int] = xs match {
      case Nil => Nil
      case Cons(h, t) => Cons[Int](h + 1, plusOne2(t))
    }

    /*
     * Exercise 3.17
     */
    def d2s(xs: List[Double]): List[String] =
      foldRight(xs, Nil: List[String])((h, t) => Cons(h.toString, t))

    /*
     * Exercise 3.18
     */
    def map[A, B](xs: List[A])(f: A => B): List[B] =
      foldRight(xs, Nil: List[B])((h, t) => Cons(f(h), t))

    def mapM[A, B](xs: List[A])(f: A => B): List[B] = {
      var buf: List[B] = Nil: List[B]

      def loop(xs: List[A]): Unit = xs match {
        case Nil => ()
        case Cons(h, t) =>
          buf = Cons(f(h), buf)
          loop(t)
      }

      loop(xs)
      buf
    }

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
  println("---------")
  println(List.foldRight2(xs, Nil: List[Int])((x, y) => Cons(x, y)))
  println(List.foldLeft2(xs, Nil: List[Int])((y, x) => Cons(x, y)))
  println("---------")

  println(List.append2(xs, List(9, 2)))
  println(List.append3(xs, List(9, 2)))

  println("----------------------")
  println(List.concat(List(xs, xs, xs)))
  println(List.concat2(List(xs, xs, xs)))


  println("----------------------")

  println(List.plusOne(xs))
  println(List.d2s(List(1.0, 2.0, 3.0)))

  println("----------------------")
  println(List.map(xs)(_ + 2))
}

