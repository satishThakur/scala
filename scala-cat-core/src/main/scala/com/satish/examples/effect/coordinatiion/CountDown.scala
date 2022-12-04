package com.satish.examples.effect.coordinatiion
import cats.effect.{Deferred, ExitCode, IO, IOApp, Ref}
import com.satish.examples.effect.pareffects.Debug.debug

import scala.concurrent.duration.*

trait CountDownLatch:
  def await: IO[Unit]
  def decrement: IO[Unit]


object CountDownLatch:
  def apply(n : Int) : IO[CountDownLatch] = IO(new SimpleCountDownLatch(n))
  class SimpleCountDownLatch(n: Int) extends CountDownLatch:
    val state: IO[Ref[IO,Int]] = Ref[IO].of(n)
    val event: IO[Deferred[IO,Unit]] = Deferred[IO, Unit]

    override def await: IO[Unit] = for{
      d <- event
    } yield d.get

    override def decrement: IO[Unit] = for{
      s <- state
      current <- s.updateAndGet(i => i -1)
      d <- event
      _ <- if current <= 0 then d.complete(()) else IO.unit
    } yield()

object Workers extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    val cl : IO[CountDownLatch] = CountDownLatch(5)
    (for{
      _ <- IO("before latch....").debug
      _ <- IO.parSequenceN(5)(List.fill(10)(worker(cl)))
      latch <- cl
      _ <- latch.await
      _ <- IO("post countdown latch").debug
    } yield ()).as(ExitCode.Success)

  def worker(cl : IO[CountDownLatch]) : IO[Unit] =
    for{
      _ <- IO.sleep(200.millisecond)
      _ <- IO("DONE...").debug
      c <- cl
    }yield c.decrement

