package com.pkhamutou.ch6

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}

class RNGTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks {

  test("testNonNegativeInt") {
    forAll { seed: Long =>
      val rng = RNG.Simple(seed)
      RNG.nonNegativeInt(rng)._1 should not be < (0)
    }
  }

  test("testDouble") {
    forAll { seed: Long =>
      val rng = RNG.Simple(seed)
      RNG.double(rng)._1 should (be >= 0d and be < 1d)
    }
  }

  test("testIntDouble") {
    forAll { seed: Long =>
      val rng = RNG.Simple(seed)
      val ((i, d), _) = RNG.intDouble(rng)
      d should (be >= 0d and be < 1d)
    }
  }

  test("testDoubleInt") {
    forAll { seed: Long =>
      val rng = RNG.Simple(seed)
      val ((d, i), _) = RNG.doubleInt(rng)
      d should (be >= 0d and be < 1d)
    }
  }

  test("testDouble3") {
    forAll { seed: Long =>
      val rng = RNG.Simple(seed)
      val ((d1, d2, d3), _) = RNG.double3(rng)
      d1 should (be >= 0d and be < 1d)
      d2 should (be >= 0d and be < 1d)
      d3 should (be >= 0d and be < 1d)
    }
  }

  test("testInts") {
    forAll { seed: Long =>
      val rng = RNG.Simple(seed)
      val ints = RNG.ints(3)(rng)
      ints._1 should have size 3
    }

  }
}
