package com.pkhamutou.ch13

import com.pkhamutou.ch12.Monad
import com.pkhamutou.ch13.Free.{FlatMap, Return}
import com.pkhamutou.ch7.Par
import com.pkhamutou.ch7.Par.Par

import language.higherKinds
import language.reflectiveCalls
import scala.annotation.tailrec

sealed trait Free[F[_], A] {
  def map[B](f: A => B): Free[F, B] = flatMap(f.andThen(Return(_))) //flatMap(a => Return(f(a)))
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap[F, A, B](this, f)
}

object Free {

  case class Return[F[_], A](a: A) extends Free[F, A]

  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]

  case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par, A]

  def freeMonad[F[_]]: Monad[({type f[a] = Free[F, a]})#f] = new Monad[({type f[a] = Free[F, a]})#f] {
    override def unit[A](a: => A): Free[F, A] = Return(a)

    override def flatMap[A, B](ma: Free[F, A])(f: A => Free[F, B]): Free[F, B] = ma.flatMap(f)
  }

  @tailrec def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(a) => a
    case Suspend(s) => s()
    case FlatMap(s, f) => s match {
      case Return(a) => runTrampoline(f(a))
      case Suspend(s) => runTrampoline(f(s()))
      case FlatMap(s2, f2) => runTrampoline {
        s2.flatMap(a => f2(a).flatMap(f))
      }
    }
  }

  @annotation.tailrec
  def step[F[_], A](a: Free[F, A]): Free[F, A] = a match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => a
  }

  def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] = a match {
    case Return(a) => F.unit(a)
    case Suspend(s) => s
    case FlatMap(Suspend(s), f) => F.flatMap(s)(a => run(f(a)))
    case _ => sys.error("Impossible, since `step` eliminates these cases")
  }

  sealed trait Console[A] {
    def toPar: Par[A]

    def toThunk: () => A
  }

  case object ReadLine extends Console[Option[String]] {
    override def toPar: Par[Option[String]] = Par.lazyUnit(run)

    override def toThunk: () => Option[String] = () => run

    def run: Option[String] = try Some(scala.io.StdIn.readLine) catch {
      case _: Exception => None
    }
  }

  case class PrintLine(line: String) extends Console[Unit] {
    override def toPar: Par[Unit] = Par.lazyUnit(println(line))

    override def toThunk: () => Unit = () => println(line)
  }

  object Console {
    type ConsoleIO[A] = Free[Console, A]

    def readLn: ConsoleIO[Option[String]] =
      Suspend(ReadLine)

    def printLn(line: String): ConsoleIO[Unit] =
      Suspend(PrintLine(line))
  }

  /* Translate between any `F[A]` to `G[A]`. */
  trait Translate[F[_], G[_]] {
    def apply[A](f: F[A]): G[A]
  }

  type ~>[F[_], G[_]] = Translate[F, G] // gives us infix syntax `F ~> G` for `Translate[F,G]`

  implicit val function0Monad = new Monad[Function0] {
    override def unit[A](a: => A): () => A = () => a

    override def flatMap[A, B](a: () => A)(f: A => (() => B)): () => B = () => f(a())()
  }

  implicit val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = Par.fork {
      Par.flatMap(a)(f)
    }
  }

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(
    implicit G: Monad[G]): G[A] =
    step(free) match {
      case Return(a) => G.unit(a)
      case Suspend(r) => t(r)
      case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _ => sys.error("Impossible, since `step` eliminates these cases")
    }

  val consoleToFunction0 = new (Console ~> Function0) {
    def apply[A](a: Console[A]) = a.toThunk
  }
  val consoleToPar = new (Console ~> Par) {
    def apply[A](a: Console[A]) = a.toPar
  }

  def runConsoleFunction0[A](a: Free[Console, A]): () => A =
    runFree[Console, Function0, A](a)(consoleToFunction0)

  def runConsolePar[A](a: Free[Console, A]): Par[A] =
    runFree[Console, Par, A](a)(consoleToPar)

  def translate[F[_], G[_], A](f: Free[F, A])(fg: F ~> G): Free[G, A] = {
    type FreeG[A] = Free[G, A]
    val t = new (F ~> FreeG) {
      def apply[A](a: F[A]): Free[G, A] = Suspend {
        fg(a)
      }
    }
    runFree(f)(t)(freeMonad[G])
  }

  def runConsole[A](a: Free[Console, A]): A =
    runTrampoline {
      translate(a)(new (Console ~> Function0) {
        def apply[A](c: Console[A]) = c.toThunk
      })
    }
}
