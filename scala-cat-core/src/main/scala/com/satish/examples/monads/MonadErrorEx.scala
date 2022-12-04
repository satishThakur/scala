package com.satish.examples.monads
import cats.MonadError
import cats.syntax.applicative.*

object MonadErrorEx extends App:
  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable ]): F[Int] =
    if age < 10 then me.pure(age) else me.raiseError(new IllegalArgumentException("age > 18"))


