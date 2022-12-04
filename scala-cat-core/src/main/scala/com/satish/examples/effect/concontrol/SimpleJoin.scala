package com.satish.examples.effect.concontrol

import cats.effect.{ExitCode, IO, IOApp}
import scala.concurrent.duration.*
import com.satish.examples.effect.pareffects.Debug.*


object SimpleJoin extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    (for{
      f <- longTask.start
      _ <- IO("foreground task").debug
      s <- f.join
      _ <- IO("after join...").debug
    } yield s).as(ExitCode.Success)

  def longTask: IO[String] =
    IO.sleep(2.second).as("long task").debug

