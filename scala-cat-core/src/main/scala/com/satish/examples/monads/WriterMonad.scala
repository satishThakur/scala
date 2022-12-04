package com.satish.examples.monads

object WriterMonad:
  import cats.data.Writer
  import cats.syntax.applicative.*
  import cats.syntax.writer.*

  type LogStrings[A] = Writer[Vector[String], A]

  def slowly[A](exp: => A): A =
    Thread.sleep(100)
    exp

  def factorial(n: Int): LogStrings[Int] =
    for{
      x <- slowly(if n == 0 then 1.pure[LogStrings] else factorial(n - 1).map(x => x * n))
      _ <- Vector(s"fact $n   $x").tell
    }yield x


object WriterMonadMain extends App:
  import WriterMonad.*
  import scala.concurrent.*
  import scala.concurrent.ExecutionContext.Implicits.*
  import scala.concurrent.duration.*

  val result = Await.result(Future.sequence(Vector(
    Future(factorial(5)),
    Future(factorial(5)),
    Future(factorial(5)),
  )).map(_.map(_.written)), 5.seconds)


  //val result = factorial(5)
  println(result)



