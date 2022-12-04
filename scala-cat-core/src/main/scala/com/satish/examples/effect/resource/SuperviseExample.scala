package com.satish.examples.effect.resource

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Supervisor

import scala.concurrent.duration.*

object SuperviseExample extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    val program : IO[Nothing] = (IO.println("hello world") *> IO.sleep(200.millisecond)).foreverM

    val p = program.guaranteeCase(IO.println(_))

    val se = Supervisor[IO].use(sv => sv.supervise(p)).void

    se.as(ExitCode.Success)



