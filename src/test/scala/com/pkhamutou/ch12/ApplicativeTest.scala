package com.pkhamutou.ch12

import org.scalatest.{FunSuite, Matchers}

class ApplicativeTest extends FunSuite with Matchers {
  test("Traverse.zipWithIndex") {
    val F = Traverse.listTraverse
    val xs = List(1, 2, 3)
    F.zipWithIndex(xs) should be (List((1, 0), (2, 1), (3, 2)))
  }

  test("Traverse.toList") {
    val F = Traverse.optionTraverse
    val xs = Option(1)
    F.toList(xs) should be (List(1))
  }

  test("Traverse.reverse") {
    val F = Traverse.listTraverse
    val xs = List(1, 2, 3, 5)
    F.reverse(xs) should be (xs.reverse)
  }
}
