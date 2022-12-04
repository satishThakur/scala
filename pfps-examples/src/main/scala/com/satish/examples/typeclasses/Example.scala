package com.satish.examples.typeclasses
import cats.Functor

trait Something[F[_]]:
  def get: F[String]


object Something:
  def make[F[_] : Functor](elem: String): F[Something[F]] = ???

object Example extends App:
  println("hello world!!")
