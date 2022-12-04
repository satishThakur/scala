package com.satish.examples.effect.catsio

import cats.effect.*
import scala.concurrent.duration.*

import scala.concurrent.duration.{Duration, FiniteDuration}
object TickingClock extends IOApp:
  def run(args: List[String]): IO[ExitCode] =
    tickingClock.as(ExitCode.Success)

  //print tick every second
  val tickingClock: IO[Unit] = IO.whenA(true){
    for{
      _ <- IO.sleep(1.seconds)
      _ <- IO(println(System.currentTimeMillis()))
      _ <- tickingClock
    } yield ()
  }

