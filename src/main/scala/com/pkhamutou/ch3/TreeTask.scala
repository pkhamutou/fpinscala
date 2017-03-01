package com.pkhamutou.ch3

object TreeTask extends App {

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    /*
     * Exercise 3.25
     */
    def size[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

    /*
     * Exercise 3.26
     */
    def maximum(tree: Tree[Int]): Int = tree match {
      case Leaf(x) => x
      case Branch(l, r) => maximum(l).max(maximum(r))
    }

    /*
     * Exercise 3.27
     */
    def depth[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + depth(l).max(depth(r))
    }

    /*
     * Exercise 3.28
     */
    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case Leaf(x) => Leaf(f(x))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    /*
     * Exercise 3.29
     */
    def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
      case Leaf(a) => f(a)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

  }

  val tree = Branch(
    Branch(Leaf(1), Leaf(2)),
    Branch(Leaf(3), Branch(Leaf(4), Leaf(5)))
  )

  println(Tree.size(tree))
  println(Tree.maximum(tree))
  println(Tree.depth(tree))
  println(Tree.map(tree)(_ + 3))
  println("+++++++++++++++++")
}
