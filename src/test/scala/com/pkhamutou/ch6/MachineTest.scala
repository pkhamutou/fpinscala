package com.pkhamutou.ch6

import org.scalatest.{FunSuite, Matchers}

class MachineTest extends FunSuite with Matchers {

  val m = Machine(locked = true, 5, 10)
  val ops = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
  test("testMachine") {
    val ((coins, candies), _) = Machine.simulateMachine(ops).run(m)
    coins should be (14)
    candies should be (1)
  }

  test("testMachine2") {
    val ((coins, candies), _) = Machine.simulateMachine2(ops).run(m)
    coins should be (14)
    candies should be (1)
  }

}
