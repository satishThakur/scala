package com.satish.examples.effect.coordinatiion
import cats.effect.{ExitCode, IO, IOApp, Ref}
import com.satish.examples.effect.pareffects.Debug.*

object SimpleRef extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    val counter : IO[Ref[IO, Long]] = Ref[IO].of(5L)
    val num : IO[Long] = (for{
      n <- counter
    } yield n.get).flatten.debug
    num.as(ExitCode.Success)



