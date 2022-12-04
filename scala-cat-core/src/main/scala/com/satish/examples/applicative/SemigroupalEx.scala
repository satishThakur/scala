package com.satish.examples.applicative

import cats.Semigroupal
import cats.instances.option.*
import cats.syntax.apply.*


object SemigroupalEx extends App:
  val x : Option[(Int, Int)] = Semigroupal[Option].product(Some(3), Some(5))
  println(x)

  val y = (Option(5), Option(7)).tupled
  println(y)

  case class Cat(name: String, age: Int, color: String)

  val olive: Option[Cat] = (Option("olive"), Option(2), Option("black")).mapN(Cat(_,_,_))
  println(olive)


  val a = Option(4)
  val b = Option("name")

  val c = Option(true)

  val S = Semigroupal[Option]
  val left  = S.product(S.product(a,b), c)

  println(left)

  val right = S.product(a, S.product(b, c))
  println(right)

  type ErrorOr[A] = Either[Vector[String], A]

  val result1 : ErrorOr[Int] = Left(Vector("error 1"))
  val result2 : ErrorOr[Int] = Left(Vector("error 2"))

  val product : ErrorOr[(Int, Int)]  = Semigroupal[ErrorOr].product(result1, result2)

  println(product)

  (Option(1), Option(2)).mapN(_ + _)