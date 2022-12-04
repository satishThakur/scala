package com.satish.examples.applicative
import cats.Applicative
import cats.instances.list.*
import cats.instances.future.*
import cats.syntax.applicative.*

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import cats.syntax.apply.*
import scala.concurrent.duration.*

//import scala.concurrent.

object TraverseOps:
  def listTraverse[F[_] : Applicative, A, B](la: List[A])(f : A => F[B]): F[List[B]] =
    // acc -> F[List[B]], a -> A
    la.foldLeft(List.empty[B].pure[F])( (acc, a) =>
      (acc, f(a)).mapN(_ :+ _)
    )

  def listSequence[F[_] : Applicative, B](la: List[F[B]]): F[List[B]] =
    listTraverse(la)(identity)




object TraverseEx extends App:
  println("hello!!")
  val l : List[Int] = 1.pure[List]
  println(l)
  val f : Future[List[Int]] = List.empty[Int].pure[Future]

  //*******
  val hostNames = List(
    "www.example.com",
    "https://myorg.org",
    "www.google.com"
  )

  def hostPing(hostName: String): Future[Int] = Future(hostName.length * 3)

  import TraverseOps.*
  val pingTimes : Future[List[Int]] = listTraverse(hostNames)(hostPing)

  val result: List[Int] = Await.result(pingTimes, 1.second)
  println(result)

  import cats.instances.vector.*

  val nestedResult = listSequence(List(Vector(1,2), Vector(3,4)))
  println(nestedResult)
  println(listSequence(List(Vector(1,2), Vector(3,4), Vector(5,6))))

  //validated
  import cats.data.Validated
  import cats.instances.list.* //monoid for list to combine 2 lists.

  type ErrorOr[A] = Validated[List[String], A]

  def process(ls : List[Int]) : ErrorOr[List[Int]] =
    listTraverse(ls) { n =>
      if n % 2 == 0 then Validated.Valid(n) else Validated.Invalid(List(s"$n is not even"))
    }

  println(process(List(1,2,3,4,5)))

  println(process(List(2,4,6, 8, 10)))

  //traverse with options
  import cats.instances.option.*

  def processOptions(ls : List[Int]) : Option[List[Int]] =
    listTraverse(ls){ n =>
      if n % 2 == 0 then Some(n) else None
    }

  println(processOptions(List(1,2,3,4,5)))
  println(processOptions(List(2,4,6)))

  //using cats native traverse now..
  import cats.Traverse
  import cats.syntax.traverse.*

  val allPings : Future[List[Int]] = hostNames.traverse(hostPing)
  println(Await.result(allPings, 1.second))

  val validResult: ErrorOr[List[Int]] = List(1,2,3,4,5).traverse(n => if n % 2 == 0 then Validated.Valid(n) else Validated.Invalid(List(s"$n is not even")))
  println(validResult)