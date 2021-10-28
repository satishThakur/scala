package chap9

import java.util.regex.Pattern

import chap8.{Gen, Prop}

import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+_]] {
  self =>

  //Execute the parser to produce the result.
  def run[A](p : Parser[A])(s: String): Either[ParseError, A]

  //create a Char Parser
  def char(c: Char): Parser[Char] = string(c.toString).map(s => s.charAt(0))



  def map[A,B](p: Parser[A])(f: A => B): Parser[B] =
    flatMap(p)(a => succeed(f(a)))

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  //Empty string is substring of every other string hence always success and produces A.
  //This is kind of Unit function for parser here!!
  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  //portion of input string p examines!!
  def slice[A](p: Parser[A]): Parser[String]

  //p1 and then p2
  def product[A,B](p1: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    flatMap(p1)(a => p2.map(b => (a,b)))


  def map2[A,B,C](p1: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    flatMap(p1)(a => p2.map(b => f(a,b)))

  //0 or more repetitions.
  def many[A](p : Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  //1 or more repetitions.
  def many1[A](p : Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  //tries to run the parser N times.
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if(n == 0) succeed(List())
    else{
      map2(p, listOfN(n-1,p))(_ :: _)
    }
  }

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  //String parser - what does it mean? it runs over string s' and if s' starts with s parsing is success
  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex): Parser[String]

  implicit def operators[A](p: Parser[A]): ParseOps[A] = ParseOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParseOps[String] =
    ParseOps(f(a))

  def whitespace: Parser[String] = "\\s*".r

  def attempt[A](p: Parser[A]): Parser[A]

  /** Parser which consumes reluctantly until it encounters the given string. */
  def thru(s: String): Parser[String] = (".*?"+Pattern.quote(s)).r

  def quoted: Parser[String] = string("\"") *> thru("\"").map(_.dropRight(1))

  def escapedQuoted: Parser[String] =
  // rather annoying to write, left as an exercise
  // we'll just use quoted (unescaped literals) for now
    token(quoted)

  def skipL[A,B](p1: Parser[A], p2: Parser[B]): Parser[B] =
    map2(slice(p1), p2)((_,b) => b)

  def skipR[A,B](p1: Parser[A], p2: Parser[B]): Parser[A] =
    map2(p1, slice(p2))((a,_) => a)

  /** Wraps `p` in start/stop delimiters. */
  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]) =
    start *> p <* stop

  /** Zero or more repetitions of `p`, separated by `p2`, whose results are ignored. */
  def sep[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] = // use `Parser[Any]` since don't care about result type of separator
    sep1(p,p2) or succeed(List())

  /** One or more repetitions of `p`, separated by `p2`, whose results are ignored. */
  def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    map2(p, many(p2 *> p))(_ :: _)

  def doubleString: Parser[String] =
    token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)

  def double: Parser[Double] =
    doubleString.map(_.toDouble)

  /** Attempts `p` and strips trailing whitespace, usually used for the tokens of a grammar. */
  def token[A](p: Parser[A]): Parser[A] =
    attempt(p) <* whitespace

  /** A parser that succeeds when given empty input. */
  def eof: Parser[String] =
    regex("\\z".r)

  /** The root of the grammar, expects no further input following `p`. */
  def root[A](p: Parser[A]): Parser[A] =
    p <* eof


  case class ParseOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def <*[B](p2: Parser[B]): Parser[A] = self.skipR(p, p2)
    def *>[B](p2: Parser[B]): Parser[B] = self.skipL(p, p2)

    def as[B](b : B): Parser[B] =
      self.map(self.slice(p))(_ => b)

    def sep(separator: Parser[Any]) = self.sep(p, separator)
    def sep1(separator: Parser[Any]) = self.sep1(p, separator)
    def **[B](p2: => Parser[B]): Parser[(A,B)] =
      self.product(p,p2)

  }

  object Laws{

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p,p.map(a => a))(in)

  }

}


