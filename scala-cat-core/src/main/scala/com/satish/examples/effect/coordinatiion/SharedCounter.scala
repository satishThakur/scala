package com.satish.examples.effect.coordinatiion
import cats.effect.{ExitCode, IO, IOApp, Ref, Deferred}
import cats.syntax.parallel.*
import scala.concurrent.duration.*
import com.satish.examples.effect.pareffects.Debug.*


object SharedCounter extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    (for{
      c <- Ref[IO].of(0L)
      d <- Deferred[IO, Unit]
      printer = countPrinter(c)
      nUpdators = IO.parSequenceN(5)(List.fill(5)(counterUpdater(c, d)))
      _ <- (printer, nUpdators, alterOnCount(d)).parTupled
    } yield ()).as(ExitCode.Success)


  def alterOnCount(d : Deferred[IO, Unit]) : IO[Unit] =
    /*
    for{
      _ <- d.get
      _ <- IO("Alert!!!").debug
    } yield ()
    */
    //d.get.map(_ => IO("Alert!!").debug).void ==> Explain why get will not work here!!
    d.get.flatMap(_ => IO("Alert!!").debug).void

  def countPrinter(c : Ref[IO, Long]) : IO[Unit] =
    for{
      n <- c.get
      _ <- IO(println(n))
      _ <- IO.sleep(1.second)
      _ <- countPrinter(c)
    } yield ()

  def counterUpdater(c: Ref[IO, Long], d : Deferred[IO,Unit]) : IO[Unit] =
    for{
      updatedVal <- c.updateAndGet(n => n + 1)
      _ <- if updatedVal >= 50 then d.complete(()) else IO.unit
      _ <- IO.sleep(1.second)
      _ <- counterUpdater(c, d)
    } yield ()