package com.satish.examples.effect.scheduler
import cats.effect.{Deferred, ExitCode, Fiber, FiberIO, IO, IOApp, Outcome, Ref, Resource}
import cats.effect.kernel.Outcome.Succeeded
import cats.data.Chain
import cats.Traverse.*
import cats.instances.option.*
import cats.syntax.traverse.*
import scala.concurrent.duration.*

import java.util.UUID

sealed trait Job

object Job:
  case class Id(id : UUID) extends AnyVal

  case class Scheduled(id: Id, task: IO[Unit]) extends Job:
    def start : IO[Running] = for{
      d <- Deferred[IO, Outcome[IO, Throwable, Unit]]
      fb <- task.guaranteeCase(d.complete(_).void).start
    }yield Running(id, fb, d)

  case class Running(id: Id,
                     ctx: Fiber[IO, Throwable, Unit],
                     done : Deferred[IO, Outcome[IO, Throwable, Unit]]):
    def await : IO[Completed] = done.get.map(Completed(id, _))

  case class Completed(id: Id, exitCode: Outcome[IO, Throwable, Unit])

  def create(task : IO[Unit]) : IO[Scheduled] =
    IO(Id(UUID.randomUUID())).map(Scheduled(_, task))

trait JobScheduler:
  def schedule(task : IO[Unit]) : IO[Job.Id]

object JobScheduler:

  def scheduler(maxConcurrency : Int): IO[JobScheduler] =
    for{
      ref <- Ref[IO].of(State(maxConcurrency))
      zzz = Zzz.asleep
      z <- zzz
      scheduler = new JobScheduler:
        def schedule(task : IO[Unit]) : IO[Job.Id] = for{
          job <- Job.create(task)
          _ <- ref.update(s => s.enqueue(job))
          _ <- z.awake
        }yield job.id
      reactor = new Reactor.DefaultReactor(ref)

      onStart = (id: Job.Id) => IO.unit
      onComplete = (job: Job.Completed) => z.awake
      loop = (z.sleep *> reactor.whenAwake(onStart, onComplete)).foreverM
      _ <- loop.start
    } yield scheduler

  case class State(max : Int,
                   scheduled : Chain[Job.Scheduled] = Chain.empty,
                  running : Map[Job.Id, Job.Running] = Map.empty,
                   completed: Chain[Job.Completed] = Chain.empty):
    // add new job to the state.
    def enqueue(job : Job.Scheduled) : State =
      this.copy(scheduled = scheduled :+ job)

    /**
     * return scheduled job and the new state, if the job can be run
     */
    def dequeue: (State, Option[Job.Scheduled]) =
      if running.size >= max then this -> None
      else scheduled.uncons.map{
        case (head, tail) => this.copy(scheduled = tail) -> Some(head)
      }.getOrElse(this -> None)

    def running(job: Job.Running): State = this.copy(running = running + (job.id -> job))

    def complete(job: Job.Completed): State = this.copy(running = running.removed(job.id),completed = completed :+ job)


trait Reactor:
  def whenAwake(onStart: Job.Id => IO[Unit],
                onComplete: Job.Completed => IO[Unit]): IO[Unit]

object Reactor:
  class DefaultReactor(ref: Ref[IO,JobScheduler.State]) extends Reactor:
    override def whenAwake(onStart: Job.Id => IO[Unit],
                           onComplete: Job.Completed => IO[Unit]): IO[Unit] =

      def tryRunning : IO[Option[Job.Running]] = for {
        st <- ref.get
        _ <- IO(println("try-running.." + st.scheduled.size))
        job : Option[Job.Scheduled] <- ref.modify(_.dequeue)
        running <- job.traverse(startJob)
      } yield running

      def startJob(job : Job.Scheduled) : IO[Job.Running] = for{
        r <- job.start
        _ <- ref.update(_.running(r))
        _ <- onStart(job.id).attempt
        _ <- forkComplete(r)
      } yield r

      def forkComplete(job: Job.Running): IO[FiberIO[Unit]] =
        (for{
          cj <- job.await
          _ <- ref.update(_.complete(cj))
          _ <- IO(println("---job Completed---"))
          _ <- onComplete(cj)
          _ <- IO(println("---job Completed SS---"))
        } yield ()).start
        //job.await.flatMap(onComplete).start

      tryRunning.iterateUntil(_.isEmpty).void



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
      _ <- if state.sleep then sleepToAwake else IO.println("Not sleeping.. Ignore")
    } yield ()

    def sleepToAwake : IO[Unit] = for{
      _ <- IO(println("sleep_to_awake"))
      newSignal <- Deferred[IO,Unit]
      oldState <- ref.getAndUpdate(s => s.copy(sleep = false, newSignal))
      _ <- oldState.signal.complete(())
      _ <- IO(println("sent signals to sleepers"))
    }yield ()


object App extends IOApp:
  override def run(args: List[String]): IO[ExitCode] = (for{
    scheduler <- JobScheduler.scheduler(5)
    _ <- scheduler.schedule(IO(println("1**hello world**")))
    _ <- scheduler.schedule(IO(println("2**hello world**")))
    _ <- scheduler.schedule(IO(println("3**hello world**")))
    _ <- IO.sleep(5.second)
    _ <- IO(println("Done...!!"))
  } yield ()).as(ExitCode.Success)
