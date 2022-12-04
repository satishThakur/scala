package com.satish.examples.effect.pareffects
import cats.effect.IO
import cats.effect.*

object Debug:
  extension[A] (io : IO[A])
    def debug: IO[A] = for{
      a <- io
      tn = Thread.currentThread().getName
      _ <- IO(println(s"[${Colorize.reversed(tn)}] - $a"))
    } yield a
