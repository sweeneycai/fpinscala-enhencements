package fpinscala.parsing

import fpinscala.testing._

import scala.util.matching.Regex

trait Parsers[Parser[+_]] {
  self => // so inner classes may call methods of trait

  // when p1 success, p2 will never be evaluated, so p2 should be non-strict
  implicit def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  def map[A, B](p: Parser[A])(f: A => B): Parser[B]

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  def slice[A](p: Parser[A]): Parser[String]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)]

  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    product(p1, p2).map(f.tupled)

  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n < 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)

  def regex(r: Regex): Parser[String]

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def productViaMap2[A, B](p1: Parser[A], p2: Parser[B]): Parser[(A, B)] =
    map2(p1, p2)((_, _))

  def productViaFlatMap[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    p1.flatMap(a => map(p2)(b => (a, b)))

  def productViaFlatMap2[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    for {a <- p1; b <- p2} yield (a, b)

  def map2ViaFlatMap[A, B, C](p1: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] =
    for {a <- p1; b <- p2} yield f(a, b)

  def map2ViaFlatMap2[A, B, C](p1: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] =
    p1.flatMap(a => p2.map(b => f(a, b)))

  // Actually, this implementation calls `map` because of `yield`
  def mapViaFlatMap[A, B](p: Parser[A])(f: A => B): Parser[B] =
    for {a <- p} yield f(a)

  // And this is the correct way of map
  def mapViaFlatMap2[A, B](p: Parser[A])(f: A => B): Parser[B] =
    p.flatMap(f andThen succeed)

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  // Example:
  char('a').many.slice.map(_.length)

  //Example:
  char('a').many.slice.map(_.length) ** char('b').many1.map(_.length)

  // Error manipulation
  def errorLocation(e: ParseError): Location

  def errorMessage(e: ParseError): String

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def errorStack(e: ParseError): List[(Location, String)]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def many: Parser[List[A]] = self.many(p)

    def slice: Parser[String] = self.slice(p)

    def many1: Parser[List[A]] = self.many1(p)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def succeedLaw[A](a: A)(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(succeed(a))(s) == Right(a))
  }

}

case class Location(input: String, offset: Int = 0) {

  lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col: Int = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int): Location = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.linesIterator.drop(line - 1).next
    else ""
}

case class ParseError(stack: List[(Location, String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}