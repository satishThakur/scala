package com.satish.examples.effect.resource

import cats.effect.{ExitCode, IO, IOApp, Resource}

import com.satish.examples.effect.pareffects.Debug.*
import cats.instances.option.*
import cats.syntax.flatMap.*

object BasicResource extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    //optionResource.use(x => Option(x + 2))
    stringResource.use(_ => IO("INSIDE RESOURCE...").debug).as(ExitCode.Success)


  val stringResource : Resource[IO, String] = Resource.make{
    IO("allocating string").debug
  }{
    _ => IO("deallocating string..").debug.void
  }

  val optionResource : Resource[Option, Int] = Resource.make(
    Option(10)
  )(_ => Option(()))


