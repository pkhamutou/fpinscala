package com.pkhamutou.ch3

import com.pkhamutou.ch3.Main._

object Helper {
  val xs = List(1, 2, 3, 4)

  val dx = List(1.0, 2.0, 3.0, 4.0)

  def print: Unit = {

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

  }

  def print18: Unit = {
    println(List.mapM(xs)(_ + 2))
  }

  def print19: Unit = {
    println(List.filter(xs)(_ % 2 != 0))
    println(List.filterM(xs)(_ % 2 != 0))
  }

  def print20 = {
    println(List.flatMap(xs)(x => List(x, x)))
  }

  def print21 = {
    println(List.filterFM(xs)(_ % 2 != 0))

  }
  def print22_23 = {
    println(List.sumValues(xs, xs))
    println(List.zipWith(List("123"), List("321"))(_ + _))
  }
}
