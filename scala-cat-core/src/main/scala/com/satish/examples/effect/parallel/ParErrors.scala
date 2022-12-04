package com.satish.examples.effect.parallel
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.parallel.*
import com.satish.examples.effect.pareffects.Debug.*

object ParErrors extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    e1.attempt.debug *>
      e2.attempt.debug *>
      e3.attempt.debug *>
      IO.pure(ExitCode.Success)


  val ok : IO[String] = IO("OK").debug
  val ko1 : IO[String] = IO.raiseError(new RuntimeException("e1 error!!")).debug
  val ko2 : IO[String] = IO.raiseError(new RuntimeException("e2 error!!")).debug
/**
  val e1 = (ok, ko1).parMapN((_, _) => ())
  val e2 = (ko1, ok).parMapN((_, _) => ())
  val e3 = (ko1, ko2).parMapN((_, _) => ())
**/
  val e1 = (ok, ko1).parTupled.void
  val e2 = (ko1, ok).parTupled.void
  val e3 = (ko1, ko2).parTupled.void