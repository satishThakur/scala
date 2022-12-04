package com.satish.examples.monads

object EitherMonad extends App:
  import cats.Monad
  import cats.syntax.either.*
  import cats.syntax.applicative.*
  import cats.syntax.functor.*

  val x : Either[String, Int] = Right(5)

  val y : Either[String, Int] = Left("error there!!")

  val p: Either[String, Int] = for{
    n <- x
    m <- y
  } yield (n * m)

  println(p)

  val someNum = 5.asRight[String]

  someNum.fold(println, println)

