package validation

import cats.Semigroup
import cats.syntax.semigroup.*
import cats.data.Validated
import cats.syntax.apply.*
import cats.syntax.validated.*



sealed trait Predicate[E: Semigroup,A]:
  import Predicate.{Pure, And}
  def apply(a: A): Validated[E,A] = this match {
    case Pure(f) => f(a)
    case And(l, r) => (l(a), r(a)).mapN((_, _) => a)
  }
  def and(other : Predicate[E,A]): Predicate[E,A] = Predicate.And(this, other)

object Predicate:
  case class And[E: Semigroup,A](left: Predicate[E,A], right: Predicate[E,A]) extends Predicate[E,A]
  case class Pure[E: Semigroup,A](f : A => Validated[E,A]) extends Predicate[E,A]
  def pure[E: Semigroup,A](f : A => Validated[E,A]) : Predicate[E,A] = Pure(f)


object TestApp extends App:
  val c1 : Predicate[List[String], Int] = Predicate.pure(i => {
    if i < 5 then i.valid else List("value should be less than 5").invalid
  })

  val c2 : Predicate[List[String], Int] = Predicate.pure(i => {
    if i % 2 == 0 then i.valid else List("value should be even").invalid
  })

  val check = c1 and c2

  println(check(10))
  println(check(9))
  println(check(4))
