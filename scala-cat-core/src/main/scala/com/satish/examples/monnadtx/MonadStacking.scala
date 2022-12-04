package com.satish.examples.monnadtx

import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*


object MonadStacking extends App:

  import cats.data.EitherT
  import cats.data.OptionT
  import cats.Applicative.*
  import cats.syntax.applicative.*
  import cats.instances.future.*
  import scala.concurrent.ExecutionContext.Implicits.global


  //Future of Either of Option
  //Future[Either[Error, Option[A]]]

  type futureOfEither[A] = EitherT[Future, Error, A]

  type optionOfEitherFuture[A] = OptionT[futureOfEither, A]


  val sum = for{
    x <- 10.pure[optionOfEitherFuture]
    y <- 20.pure[optionOfEitherFuture]
  }yield x + y

  println(Await.result(sum.value.value, 1.second))
