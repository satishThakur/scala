package com.satish.examples.effect.pareffects
import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.unsafe.implicits.global
import cats.syntax.apply.*

object IOCompose extends IOApp:
  def threadStr(s : String) : String = s"[${Thread.currentThread().getName}] $s"

  override def run(args: List[String]): IO[ExitCode] =
    val hl = IO(println(threadStr("hello")))
    val wd = IO(println(threadStr("world")))

    val compose : IO[Unit] = for{
      _ <- hl
      _ <- wd
    } yield ()

    val compose1: IO[Unit] = (hl, wd).mapN((_, _) => ())
    (compose *> compose1).as(ExitCode.Success)


