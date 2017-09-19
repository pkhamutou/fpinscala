package com.pkhamutou.ch9

trait Parsers[Parser[+_]] { self =>

  case class ParserError(stack: List[(Location, String)])

  def run[A](p: Parser[A])(input: String): Either[ParserError, A]
  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  implicit def string(c: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(Nil)

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)


  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    flatMap(a)(f.andThen(succeed))

  def map2[A, B, C](pa: Parser[A], pb: => Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(pa)(a => map(pb)(b => f(a, b)))

  def slice[A](p: Parser[A]): Parser[String]
  def product[A, B](pa: Parser[A], pb: => Parser[B]): Parser[(A, B)] =
    flatMap(pa)(a => map(pb)(b => (a, b)))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = map2(p, succeed(n))((a, _) => List.fill(n)(a))


  object Low {
    run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
//    run(map(many(char('a')))(_.size)) == Right(1)

  }

  case class Location(input: String, offset: Int = 0) {
    lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col: Int = input.slice(0, offset + 1).lastIndexOf('\n') match {
      case -1 => offset + 1
      case lineStaer => offset - lineStaer
    }
  }

  def label[A](msg: String)(p: Parser[A]): Parser[A]
  def errorLocation(e: ParserError): Location
  def errorMessage(e: ParserError): String

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def slice: Parser[String] = self.slice(p)
    def product[B](pb: Parser[B]): Parser[(A, B)] = self.product(p, pb)
    def **[B](pb: Parser[B]): Parser[(A, B)] = self.product(p, pb)
  }
}
