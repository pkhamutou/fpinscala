package com.pkhamutou.ch4

import scala.{Option => _, Some => _, Either => _, _}

object OptionTask extends App {

  println("Hey")

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

  println(Some(321).map(_ + 2).getOrElse(123))
  println(Some(312).flatMap(x => Some(x + 3)))
  println(Some(321).orElse(Some(111)))
  println(Some(3).filter(_ > 1))
}
