package com.satish.examples.effect.coordinatiion

import cats.effect.{Deferred, ExitCode, IO, IOApp, Ref}

trait Zzz:
  def sleep: IO[Unit]

  def awake: IO[Unit]

object Zzz:
  case class State(sleep: Boolean, signal : Deferred[IO, Unit])

  def asleep: IO[Zzz] =
    for{
      d <- Deferred[IO, Unit]
      ref <- Ref[IO].of(State(true, d))
    } yield ZzzImpl(ref)


  class ZzzImpl(ref: Ref[IO, State]) extends Zzz:
    override def sleep: IO[Unit] = for{
      _ <- ref.update(s => s.copy(sleep = true))
      state <- ref.get
      _ <- IO(println("going to sleep"))
      _ <- state.signal.get
      _ <- IO(println("Out of sleep"))
    }yield ()

    override def awake: IO[Unit] = for{
      state <- ref.get
      _ <- if state.sleep then sleepToAwake else IO.unit
    } yield ()

    def sleepToAwake : IO[Unit] = for{
      _ <- IO(println("sleep_to_awake"))
      newSignal <- Deferred[IO,Unit]
      oldState <- ref.getAndUpdate(s => s.copy(sleep = false, newSignal))
      _ <- oldState.signal.complete(())
      _ <- IO(println("sent signals to sleepers"))
    }yield ()

object ZzzApp extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    (for{
      zzz <- Zzz.asleep
      fb <- zzz.sleep.start
      _ <- zzz.awake
      _ <- fb.join
      fb1 <- zzz.sleep.start
      _ <- zzz.awake
      _ <- fb1.join
    } yield ()).as(ExitCode.Success)


