package com.satish.examples.state
import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Console

object Printer:
  def print[F[_] : Console](name : String) : F[Unit] =
    val printEffect: F[Unit] = Console[F].print(name)
    printEffect



object ConsoleExample extends IOApp:
  import Printer.*
  override def run(args: List[String]): IO[ExitCode] =
    print[IO]("hello there").as(ExitCode.Success)

