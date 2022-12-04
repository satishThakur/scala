package com.satish.examples.effect.resource

import cats.effect.{ExitCode, IO, IOApp}

import scala.concurrent.duration.*


object StartCancel extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =

    val program : IO[Nothing] = (IO.println("hello world") *> IO.sleep(200.millisecond)).foreverM
    val p = program.guaranteeCase(IO.println(_))

    val another = IO(throw new RuntimeException).guaranteeCase(o => IO.println(s"done.. $o"))

    (for{
      fp <- another.start
      _ <- IO.sleep(2.second) *> fp.cancel
      _ <- fp.join
    } yield ()).as(ExitCode.Success)



