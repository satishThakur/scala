package com.satish.examples.effect.concontrol

import cats.effect.IO.{IOCont, Uncancelable}
import cats.effect.{ExitCode, IO, IOApp}
import com.satish.examples.effect.pareffects.Debug.*

import scala.concurrent.duration.*

object Timeout extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    val task = annotatedSleep(100, "task")
    val timeout = annotatedSleep(50, "timeout")

    val result: IO[String] = IO.race(task, timeout).map(_ match {
      case Left(s) => s"$s won"
      case Right(s) => s"$s won"
    })

    result.debug.as(ExitCode.Success)


  def annotatedSleep(units: Long, name : String): IO[String] =
    (for{
      _ <- IO(s"$name - started").debug
      _ <- IO.sleep(units.millisecond)
      _ <- IO(s"$name - finishes").debug
    } yield name).onCancel(IO(s"$name cancelled").debug.void)
