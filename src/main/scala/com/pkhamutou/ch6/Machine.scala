package com.pkhamutou.ch6

case class Machine(locked: Boolean, candies: Int, coins: Int)

sealed trait Input
case object Coin extends Input
case object Turn extends Input

object Machine {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    State(m => {
      val p = inputs.foldLeft(m) {
        case (z, _) if z.candies == 0 => z
        case (z, Turn) if z.locked => z
        case (z, Coin) if !z.locked => z

        case (z, Coin) => Machine(false, z.candies, z.coins + 1)
        case (z, Turn) => Machine(true, z.candies - 1, z.coins)
      }
      ((p.coins, p.candies), p)
    })
  }

  def update: (Input) => ((Machine) => Machine) = (i: Input) => (s: Machine) => (i, s) match {
    case (_, Machine(_, 0, _)) => s
    case (Coin, Machine(false, _, _)) => s
    case (Turn, Machine(true, _, _)) => s
    case (Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1)
    case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
  }

  import State._
  def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] = {
    sequence(inputs map (modify[Machine] _ compose update))
      .flatMap(_ => get.map(s => (s.coins, s.candies)))
  }

  }
