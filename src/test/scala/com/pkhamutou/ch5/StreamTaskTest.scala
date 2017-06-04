package com.pkhamutou.ch5

import org.scalatest._
import scala.{Stream => _}
import com.pkhamutou.ch5.StreamTask._
  
class StreamTaskTest extends FunSuite with Matchers {
  val stream = Stream(1, 2, 3, 4)
  test("fibs, from, constant, ones in terms of unfold") {
    println(Stream.fibs.take(5).toList)
    println(Stream.fibsUnfold.take(5).toList)

    println(Stream.from(2).take(5).toList)
    println(Stream.fromUnfold(2).take(5).toList)

    println(Stream.constant("qwe").take(3).toList)
    println(Stream.constantUnfold("qwe").take(3).toList)

    println(Stream.onesUnfold.take(4).toList)

  }

  test("map, take, takeWhile, zipWith, zipAll in terms of unfold") {
    println(stream.mapUnfold(_ + 1).take(3).toList)
    println(stream.takeUnfold(3).toList)
    println(stream.takeWhileUnfold(_ <= 3).toList)
    println(stream.zipWith(stream)(_ + _).toList)
    println(stream.zipAll(Stream(1, 2)).toList)

    stream.startsWith(Stream(1, 2)) shouldBe true
    stream.startsWith(Stream(3, 2)) shouldBe false
    stream.startsWith(Stream(1, 2, 3, 4, 5, 6)) shouldBe false

    Stream(1, 2, 3).tails.map(_.toList).toList shouldBe List(List(1, 2, 3), List(2, 3), List(3), List())

    stream.hasSubsequence(Stream(2, 3)) shouldBe true

    Stream(1, 2, 3).scanRight(0)(_ + _).toList shouldBe List(6, 5, 3, 0)

    stream.find(_ == 4) shouldBe Some(4)
    stream.find2(_ == 4) shouldBe Some(4)

  }
}
