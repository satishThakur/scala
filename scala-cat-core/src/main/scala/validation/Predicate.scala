package validation

import cats.Semigroup
import cats.syntax.semigroup.*
import cats.data.Validated
import cats.syntax.apply.*
import cats.syntax.validated.*



sealed trait Check[E: Semigroup,A]:
  import Check.{Pure, And}
  def apply(a: A): Validated[E,A] = this match {
    case Pure(f) => f(a)
    case And(l, r) => (l(a), r(a)).mapN((_, _) => a)
  }
  def and(other : Check[E,A]): Check[E,A] = Check.And(this, other)

object Check:
  case class And[E: Semigroup,A](left: Check[E,A], right: Check[E,A]) extends Check[E,A]
  case class Pure[E: Semigroup,A](f : A => Validated[E,A]) extends Check[E,A]
  def pure[E: Semigroup,A](f : A => Validated[E,A]) : Check[E,A] = Pure(f)


object TestApp extends App:
  val c1 : Check[List[String], Int] = Check.pure(i => {
    if i < 5 then i.valid else List("value should be less than 5").invalid
  })

  val c2 : Check[List[String], Int] = Check.pure(i => {
    if i % 2 == 0 then i.valid else List("value should be even").invalid
  })

  val check = c1 and c2

  println(check(10))
  println(check(9))
  println(check(4))
