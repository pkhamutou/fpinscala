package com.pkhamutou.ch4

import scala.{Option => _, Either => _, _}

object EitherTask extends App {

  sealed trait Either[+E, +A] {

    def map[B](f: A => B): Either[E, B] = this match {
      case Right(x) => Right(f(x))
      case Left(x) => Left(x)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(x) => f(x)
      case Left(x) => Left(x)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(x) => Right(x)
      case Left(_) => b
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      this.flatMap(x => b.map(y => f(x, y)))
  }

  object Either {
    def traverse[E, A, B](xs: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      xs.foldRight[Either[E, List[B]]](Right(Nil)) { (x, z) => f(x).map2(z)(_ :: _) }

    def sequence[E, A](xs: List[Either[E, A]]): Either[E, List[A]] = traverse(xs)(x => x)
  }

  case class Right[+A](value: A) extends Either[Nothing, A]
  case class Left[+E](value: E) extends Either[E, Nothing]

  val left: Either[String, Int] = Left("Error")
  val right: Either[String, Int] = Right(123)

  println(Either.traverse(List(1, 2, 3))(x => Right(x)))
  println(Either.sequence(List(right, right, left)))

}
