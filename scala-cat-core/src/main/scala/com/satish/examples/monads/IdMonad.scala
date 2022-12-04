package com.satish.examples.monads

object IdMonad extends App:
  import cats.Monad
  import cats.syntax.flatMap.*
  import cats.syntax.functor.*
  type Identity[T] = T

  given IdentityMoand: Monad[Identity] with
    override def pure[A](x: A): Identity[A] = x
    // Members declared in cats.FlatMap
    override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)
    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
      case Left(e) => tailRecM(e)(f)
      case Right(v) => v
    }

  val calc = for{
    x <- 4 : Identity[Int]
    y <- 9 : Identity[Int]
  } yield x * y

  println(calc)

