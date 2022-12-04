package com.satish.examples.effect.parallel
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.apply.*
import com.satish.examples.effect.pareffects.Debug.*
object SeqComposition extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    hw.as(ExitCode.Success)

  val hello = IO("Hello").debug
  val world = IO("world").debug

  val hw = (hello, world).mapN(_ + " " + _).debug
