package com.satish.examples.effect.queue

import cats.effect.{ExitCode, IO, IOApp, Ref, Sync, Temporal, Async}
import cats.effect.std.Console
import scala.concurrent.duration.*
import cats.syntax.all.*
import cats.Monad

object ProducerComsumers extends IOApp:
  import CQueue.*

  def producer[F[_]: Async: Console : Temporal](counter : Ref[F, Int], q: CQueue[F, Int]) : F[Unit] =
    for{
      current <- counter.updateAndGet(_ + 1)
      _ <- q.offer(current)
      _ <- Temporal[F].sleep(5.microsecond)
      _ <- if current % 1000 == 0 then Console[F].println(s"[${Thread.currentThread().getName}] [Producer] - Produced ${current} messages") else Async[F].unit
      _ <- producer(counter, q)
    }yield ()

  def consumer[F[_]: Sync: Console](q : CQueue[F, Int]): F[Unit] =
    for{
      v <- q.take
      _ <- if v %1000 == 0 then Console[F].println(s"[${Thread.currentThread().getName}] [Consumer] - Consumed ${v} messages") else Sync[F].unit
      _ <- consumer(q)
    } yield ()

  def debugger[F[_] : Monad : Temporal, A](q : CQueue[F, A]): F[Unit] =
    for{
      _ <- q.debug
      _ <- Temporal[F].sleep(2.second) *> debugger(q)
    }yield ()

  override def run(args: List[String]): IO[ExitCode] =
    for{
      q <- CQueue[IO, Int](1000)
      counter <- Ref[IO].of(0)
      p = producer(counter, q)
      c = consumer(q)
      d = debugger(q)
      res <- List(p, p, p, c , c, c, d)
        .parSequence.as(ExitCode.Success) // Run producers and consumers in parallel until done (likely by user cancelling with CTRL-C)
        .handleErrorWith { t =>
          Console[IO].errorln(s"Error caught: ${t.getMessage}").as(ExitCode.Error)
        }
    } yield res



