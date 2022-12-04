package com.satish.examples.applicative

object SemigroupalEx extends App:
  import cats.Semigroupal.*
  import cats.instances.option.*

  Semigroupal[Option].product()
