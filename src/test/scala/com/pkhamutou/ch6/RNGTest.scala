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
}