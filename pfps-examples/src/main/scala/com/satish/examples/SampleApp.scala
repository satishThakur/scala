package com.satish.examples
import cats.effect.{ExitCode, IO, IOApp}

object SampleApp extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    val x = "ddd".nonEmpty
    IO(println("Scala-3-cats-3 hello world")).as(ExitCode.Success)
