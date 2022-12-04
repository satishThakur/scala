package com.satish.examples.monads


object CatsId extends App:
  import cats.Monad
  import cats.Id
  import cats.syntax.flatMap.*
  import cats.syntax.functor.*
  import cats.syntax.applicative.*

  val x  = 4.pure[Id]

  val y = 10.pure[Id]

  val mul = for {
    a <- x
    b <- y
  }yield a * b

  println(mul)

  def multiply[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] = for{
    x <- a
    y <- b
  } yield (x * y)


  println(multiply(5: Id[Int], 10: Id[Int]))
