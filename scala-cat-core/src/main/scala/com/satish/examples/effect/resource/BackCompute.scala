package com.satish.examples.effect.resource
import cats.effect.{ExitCode, IO, IOApp, Ref}

import scala.concurrent.duration.*

object BackCompute extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =

    val counter : IO[Ref[IO, Int]] = Ref[IO].of(0)
    val program : IO[Int] = counter.flatMap(c =>
      (IO.println("hello world") *> IO.sleep(200.millisecond) *> c.updateAndGet(_ + 1)).iterateUntil(_ > 10 )).as(10)

    val p = program.guaranteeCase(IO.println(_))

    val result : IO[Unit] = p.background.use(
      value => value.flatMap(
        _ => IO.println("I am doing something") *> IO.sleep(2.second)))

    result.as(ExitCode.Success)

}
