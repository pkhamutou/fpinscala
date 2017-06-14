package com.pkhamutou.ch6

/**
  * Created by pkhamutou on 6/13/17.
  */
trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (r, s) = rng.nextInt
    (if (r < 0) -(r + 1) else r, s)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (r, s) = nonNegativeInt(rng)
    (r / (Int.MaxValue.toDouble + 1), s)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, s1) = rng.nextInt
    val (d, s2) = double(s1)
    ((i, d), s2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (r, s) = intDouble(rng)
    (r.swap, s)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, s1) = double(rng)
    val (d2, s2) = double(s1)
    val (d3, s3) = double(s2)
    ((d1, d2, d3), s3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(count: Int, rng: RNG, xs: List[Int]): (List[Int], RNG) = count match {
      case 0 => (xs, rng)
      case _ => 
        val (r, s) = rng.nextInt
        loop(count - 1, s, r :: xs)
    }
    loop(count, rng, Nil)
  }
}
