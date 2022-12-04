package com.satish.examples.effect.pareffects
import cats.effect.{ExitCode, IO, IOApp}
import Debug.*
import cats.syntax.apply.*
import cats.syntax.parallel.*

object ParIOCompose extends IOApp:


  override def run(args: List[String]): IO[ExitCode] =
    val h = IO("hello").debug
    val w = IO("world").debug
    val p = (h,w).parMapN((a,b) => s"$a : $b").debug
    p.as(ExitCode.Success)