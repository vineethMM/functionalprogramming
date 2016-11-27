package com.study.fp.chapter6

/**
 * Created by Vineeth on 22/11/16.
 */

// Exercise 6.11
sealed trait Input

case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int){
  def stateTuple: ((Int, Int), Machine) = ((coins, candies), this)
}

object Machine {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    inputs.foldLeft[State[Machine, (Int, Int)]](State(s => s.stateTuple)) {
      (s, i) => s.flatMap(_ => simulateMachine(i))
    }

  def simulateMachine(input: Input): State[Machine, (Int, Int)] =
   input match {
     case Coin => unLock
     case Turn => dispense
   }

  def unLock: State[Machine, (Int, Int)] =
    State {
      case Machine(true, candies, coins)  => Machine(false, candies, coins + 1).stateTuple
      case Machine(false, candies, coins) => Machine(false, candies, coins).stateTuple
    }

  def dispense: State[Machine, (Int, Int)] =
    State {
      case Machine(false, c, n) if c > 0 => Machine(true, c - 1, n).stateTuple
      case machine                       => machine.stateTuple
    }
}
