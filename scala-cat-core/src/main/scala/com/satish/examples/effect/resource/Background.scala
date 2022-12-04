package com.satish.examples.effect.resource
import cats.effect.{ExitCode, IO, IOApp}

import scala.concurrent.duration.*

object BackgroundApp extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =

    val program : IO[Nothing] = (IO.println("hello world") *> IO.sleep(200.millisecond)).foreverM

    val p = program.guaranteeCase(IO.println(_))

    val result : IO[Unit] = p.background.use(_ => {
      IO.println("I am doing something") *> IO.sleep(2.second)
    })

    result.as(ExitCode.Success)

}
