package com.pkhamutou.ch3

import com.pkhamutou.ch3.ListTask.{List, Cons, Nil}

object Folds {

  def foldRight[A, B](xs: List[A], z: B)(f: (A, B) => B): B = xs match {
    case Nil => z
    case Cons(x, t) => f(x, foldRight(t, z)(f))
  }

  /*
  foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)((x, z) => x + z)
  f(1, foldRight(Cons(2, Cons(3, Nil)), 0)((x, z) => x + z))
  f(1, f(2, foldRight(Cons(3, Nil), 0)((x, z) => x + z)))
  f(1, f(2, f(3, foldRight(Nil, 0)((x, z) => x + z))))
  f(1, f(2, f(3, 0))) = 6
  */

  def foldLeft[A, B](xs: List[A], z: B)(f: (B, A) => B): B = xs match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  /*
  foldLeft(Cons(1, Cons(2, Cons(3, Nil))), 0)             ((z, y) => z + y)
  foldLeft(Cons(2, Cons(3, Nil)),         (0 + 1))        ((z, y) => z + y)
  foldLeft(Cons(3, Nil),                  (0 + 1 + 2))    ((z, y) => z + y)
  foldLeft(Nil,                           (0 + 1 + 2 + 3))((z, y) => z + y)
  0 + 1 + 2 + 3 = 6
  */

  def reverse[A](xs: List[A]): List[A] = foldLeft(xs, List[A]())((acc, h) => Cons(h, acc))

  /*
  foldLeft(Cons(1, Cons(2, Cons(3, Nil))), Nil)                           ((z, a) => Cons(a, z))
  foldLeft(Cons(2, Cons(3, Nil)),          Cons(3, Nil))                  ((z, a) => Cons(a, z))
  foldLeft(Cons(3, Nil),                   Cons(2, Cons(3, Nil)))         ((z, a) => Cons(a, z))
  foldLeft(Nil,                            Cons(3, Cons(2, Cons(1, Nil))))((z, a) => Cons(a, z))
  Cons(3, Cons(2, Cons(1, Nil))))
  */

  def foldRightViaFoldLeft[A, B](xs: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(xs), z)((b, a) => f(a, b))

  def foldRightViaFoldLeft_1[A, B](xs: List[A], z: B)(f: (A, B) => B): B = {
    val s = foldLeft(xs, (b: B) => b)((g, a) => (b => g(f(a, b))))
    s(z)
  }

  /*
  val f: (Int, Int) => Int = (a, b) => a + b
  (Cons(1, Cons(2, Cons(3, Nil))), 0) ((a, b) => a + b)

  foldLeft(Cons(1, Cons(2, Cons(3, Nil))),  (b: B) => b)                      ((g, a) => (b => g(f(a, b))))
  foldLeft(Cons(2, Cons(3, Nil)),           (b: B) => f(1, b))                ((g, a) => (b => g(f(a, b))))
  foldLeft(Cons(3, Nil),                    (b: B) => f(1, f(2, b)))          ((g, a) => (b => g(f(a, b))))
  foldLeft(Nil,                             (b: B) => f(1, f(2, f(3, b))))    ((g, a) => (b => g(f(a, b))))

  val return: B => B = b: B => f(1, f(2, f(3, b)))
  return(0)

  */

  def foldLeftViaFoldRight[A, B](xs: List[A], z: B)(f: (B, A) => B): B =
    foldRight(xs, (b: B) => b)((a, g) => b => g(f(b, a)))(z)


  /*
  val f: (Int, Int) => Int = (a, b) => a + b
  (Cons(1, Cons(2, Cons(3, Nil))), 0) ((a, b) => a + b)

  foldRight(Cons(1, Cons(2, Cons(3, Nil))), b: B => b)((a, g) => b => g(f(b, a)))

  f(1, foldRight(Cons(2, Cons(3, Nil)), b: B => b)    ((a, g) => b => g(f(b, a))))
  f(1, f(2, foldRight(Cons(3, Nil), b: B => b)        ((a, g) => b => g(f(b, a))))
  f(1, f(2, f(3, foldRight(Nil, b: B => b)            ((a, g) => b => g(f(b, a))))
  f(1, f(2, f(3, b: B => b)
  
  */

  def foldRight2[A, B](xs: List[A], z: B)(f: (A, B) => B): B = xs match {
    case Nil => z
    case Cons(x, t) => f(x, foldRight(t, z)(f))
  }
}
