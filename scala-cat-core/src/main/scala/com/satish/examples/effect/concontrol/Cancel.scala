package com.satish.examples.effect.concontrol

import cats.effect.{ExitCode, IO, IOApp}
import scala.concurrent.duration.*
import com.satish.examples.effect.pareffects.Debug.*
object Cancel extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    (for{
      bg <- (IO.sleep(3.second) *> IO("LONG TASK").debug).onCancel(IO("Cancelled..").debug.void).start
      _ <- IO.sleep(5.second) *> bg.cancel
    }yield ()).as(ExitCode.Success)
