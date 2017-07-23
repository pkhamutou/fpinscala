package com.pkhamutou.ch7

import java.util.concurrent.{ExecutorService, ForkJoinPool}

import org.scalatest.{FunSuite, Matchers}

class ParTest extends FunSuite with Matchers  {

  val pool: ExecutorService = ForkJoinPool.commonPool()

  test("testParFilter") {
    Par.parFilter(List(1, 2, 3))(x => x != 2)(pool).get shouldBe List(1, 3)
  }

  test("testUnit") {
    Par.unit(2)(pool).get shouldBe 2
  }

  test("testLazyUnit") {
    Par.lazyUnit(2)(pool).get shouldBe 2
  }

}
