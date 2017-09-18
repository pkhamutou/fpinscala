package com.pkhamutou.ch8

import com.pkhamutou.ch6.{RNG, State}
import com.pkhamutou.ch5.StreamTask._
import com.pkhamutou.ch8.Prop._


object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\ngenerated and exception: ${e.getMessage}\nstack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
    randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }
}

case class Prop(run: (TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = ???
}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  val isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  val isFalsified = true
}

case class Gen[+A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(x => f(x).sample))
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(s => Gen(State.sequence(List.fill(s)(this.sample))))

  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen[Int](State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def unit[A](a: => A): Gen[A] = Gen[A](State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(if(_) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(State(RNG.double).flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }
}

case class SGen[+A](forSize: Int => Gen[A])
