package com.pkhamutou.ch2

object Main extends App {
  println("Hey")
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
}
