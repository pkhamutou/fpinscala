package com.pkhamutou.ch8

import com.pkhamutou.ch6.RNG
import org.scalatest.{FunSuite, Matchers}

class GenTest extends FunSuite with Matchers {

  val rng = RNG.Simple(42)

  test("testChoose") {
    val s = 1
    val f = 10
    val result = Gen.choose(s, f).sample.run(rng)._1
    result should be <= f
    result should be > 1
  }

  test("testUnit") {
    Gen.unit(2).sample.run(rng)._1 shouldBe 2
  }

  test("testBoolean") {
    Gen.boolean.sample.run(rng)._1 should (be(true) or be(false))
  }

  test("testListOnN") {
    Gen.listOfN(3, Gen.unit(2)).sample.run(rng)._1 shouldBe List(2, 2, 2)
  }

}
