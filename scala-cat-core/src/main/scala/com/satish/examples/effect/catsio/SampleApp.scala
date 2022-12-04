package com.satish.examples.effect.catsio
import cats.effect.{ExitCode, IO, IOApp}

object SampleApp extends IOApp:

  override def run(args: List[String]): IO[ExitCode] =
    val message : IO[Unit] = IO(println("hello world"))
    message.as(ExitCode.Success)


