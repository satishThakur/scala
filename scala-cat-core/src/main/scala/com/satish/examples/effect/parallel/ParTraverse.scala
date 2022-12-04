package com.satish.examples.effect.parallel
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.traverse.*
import cats.syntax.parallel.*
import com.satish.examples.effect.pareffects.Debug.*

object ParTraverse extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    val task : Int => IO[Int] = IO(_).debug
    val nums = (1 to 50).toList
    nums.parTraverse(task).debug.as(ExitCode.Success)


