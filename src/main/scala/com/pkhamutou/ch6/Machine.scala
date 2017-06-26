package com.pkhamutou.ch6

case class Machine(locked: Boolean, candies: Int, coins: Int)

sealed trait Input
case object Coin extends Input
case object Turn extends Input

object Machine {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
