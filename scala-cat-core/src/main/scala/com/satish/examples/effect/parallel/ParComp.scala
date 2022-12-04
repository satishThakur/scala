package com.satish.examples.effect.parallel
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.parallel.*
import com.satish.examples.effect.pareffects.Debug.*

object ParComp extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    hw.as(ExitCode.Success)

  val hello = IO("Hello").debug
  val world = IO("World").debug
  val something = IO.blocking({println("whats happening!!"); "something"})

  val hw = (hello, world, something).parMapN(_ + " " + _ + " " + _).debug
