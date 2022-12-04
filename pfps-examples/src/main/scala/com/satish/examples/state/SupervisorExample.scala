package com.satish.examples.state
import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.{Semaphore, Supervisor}

object SupervisorExample extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =

    (IO.println(s"[${Thread.currentThread().getName}] start") *> Supervisor[IO].use(s => s.supervise(IO.println(s"[${Thread.currentThread().getName}] hello there.."))).void).as(ExitCode.Success)
