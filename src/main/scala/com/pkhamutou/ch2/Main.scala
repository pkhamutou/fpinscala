package com.pkhamutou.ch2

object Main extends App {
  /*
   * Exercise 2.1
   */
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, curr: Int): Int = {
      if (n == 0) prev
      else loop(n - 1, curr, curr + prev)
    }
    loop(n, 0, 1)
  }

  println(fib(6))

  /*
   * Exercise 2.2
   */
  def isSorted[A](as: Array[A], f: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(index: Int): Boolean = {
      if (index == as.length - 1) true
      else if (!f(as(index), as(index + 1))) false
      else loop(index + 1)
    }
    loop(0)
  }

  println(isSorted[Int](Array(1, 2, 3), (x, y) => x < y))
  println(isSorted[Int](Array(9, 1), (x, y) => x < y))

  /*
   * Exercise 2.3
   */
  def curry1[A, B, C](f: (A, B) => C): A => (B => C) = a => f(a, _)
  def curry2[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  curry1[Int, Int, Int]((a, b) => a + b)
  curry2[Int, Int, Int]((a, b) => a + b)

 /*
  * Exercise 2.4
  */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  println(uncurry[Int, Int, Int](a => b => a + b + 3)(1, 2))

 /*
  * Exercise 2.5
  */
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
  
  println(compose[Int, Int, Int](f => f + 1, g => g + 3)(1))

}

