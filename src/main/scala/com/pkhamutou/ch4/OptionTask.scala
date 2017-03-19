package com.pkhamutou.ch4

import scala.{Option => _, Some => _, Either => _, _}

object OptionTask extends App {

  sealed trait Option[+A] {

    def map[B](f: A => B): Option[B] = this match {
      case Some(x) => Some(f(x))
      case None => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(x) => x
      case None => default
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

    def filter(f: A => Boolean): Option[A] = flatMap(x => if (f(x)) Some(x) else None)

  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  object Option {
    def variance(xs: Seq[Double]): Option[Double] =
      mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
        a.flatMap(aa => b.map(bb => f(aa, bb)))

    def sequence[A](xs: List[Option[A]]): Option[List[A]] =
      xs.foldRight(Some(List.empty[A]): Option[List[A]]) { (x, z) => z.flatMap(ls => x.map(_ :: ls)) }

    def traverse[A, B](xs: List[A])(f: A => Option[B]): Option[List[B]] =
      xs.foldRight[Option[List[B]]](Some(Nil)) { (x, z) => map2(f(x), z)(_ :: _) }

    def sequence2[A](xs: List[Option[A]]): Option[List[A]] = traverse(xs)(x => x)
  }

}
