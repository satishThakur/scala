package com.satish.examples.effect.shift
import cats.effect.{ExitCode, IO, IOApp}
import com.satish.examples.effect.pareffects.Debug.*


object Blocking extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    (for{
      _ <- IO("before blocking..").debug
      _ <- IO.blocking{
        println(s"[${Thread.currentThread().getName}] - Inside blocking")
        "blocking..."
      }.debug
      _ <- IO("Post Blocking..")

    }yield ()).as(ExitCode.Success)
