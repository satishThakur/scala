package com.satish.examples.state

import cats.effect.{ExitCode, IO, IOApp}
import scala.concurrent.duration.*


object SupervisorEx1 extends IOApp:

  override def run(args: List[String]): IO[ExitCode] =
    val program = IO.sleep(1.second) >> IO.println("Iam done...")
    program.start.flatMap(
      fiber => fiber.join).
      timeout(5000.millisecond).
      void.as(ExitCode.Success)
