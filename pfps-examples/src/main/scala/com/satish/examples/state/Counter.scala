package com.satish.examples.state

import cats.effect.{ExitCode, IO, IOApp, Ref}
import cats.effect.Sync
import cats.syntax.functor.*
import cats.Functor
trait Counter[F[_]]:
  def get: F[Int]
  def incr: F[Unit]

object Counter:
  def make[F[_]: Ref.Make : Functor]: F[Counter[F]]  =
    val r : F[Ref[F, Int]] = Ref[F].of(0)
    r.map(ref => new Counter[F] {
    override def get: F[Int] = ref.get
    override def incr: F[Unit] = ref.update(_ + 1)
  })

object CounterApp extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    (for{
      counter <- Counter.make[IO]
      _ <- counter.get.flatMap(IO.println(_))
      _ <- counter.incr *> counter.incr
      _ <- counter.get.flatMap(IO.println(_))
    } yield ()).as(ExitCode.Success)
