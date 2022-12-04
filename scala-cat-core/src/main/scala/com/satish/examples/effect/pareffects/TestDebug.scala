package com.satish.examples.effect.pareffects
import cats.effect.{ExitCode, IO, IOApp}
object TestDebug extends IOApp:
  import Debug.*

  override def run(args: List[String]): IO[ExitCode] =
    val effect = for{
      _ <- IO(println("Hello world")).debug
    } yield ()
    effect.as(ExitCode.Success)


