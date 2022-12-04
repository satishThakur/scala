package com.satish.examples.effect.queue

import cats.effect.{Async, Deferred, ExitCode, IO, IOApp, Ref, Sync}
import cats.effect.std.Console

import scala.collection.immutable.Queue
import scala.concurrent.duration.*
import cats.syntax.all.*
import cats.effect.syntax.all.*

import java.util.concurrent.atomic.AtomicInteger

//should ideally be co-variant - explore this
//basic questions why does queue need to be effectful - few thoughts:
//we are modeling a queue which can block the callers and even the consumers - such effects are modelled in effect system
trait CQueue[F[_], A]:
  def offer(a: A): F[Unit]
  def take: F[A]
  def debug: F[Unit]


object CQueue:

  def apply[F[_]: Async : Console,A](size: Int): F[CQueue[F,A]] =
    Ref[F].of(State[F,A]).map(ref => new Unbounded[F,A](ref, size))

  case class State[F[_],A](queue: Queue[A],takes : Queue[Deferred[F, A]], offers: Queue[(A, Deferred[F, Unit])])

  object State:
    def apply[F[_],A]: State[F,A] = State[F,A](Queue.empty, Queue.empty, Queue.empty)

  class Unbounded[F[_]: Async: Console, A](val state : Ref[F, State[F,A]], val size : Int) extends CQueue[F, A]:

    override def debug: F[Unit] =
      for{
        s <- state.get
        _ <- Console[F].println(s"${Thread.currentThread().getName}] " +
          s"Debug: Queue size - ${s.queue.size}, " +
          s"Takes Queue size - ${s.takes.size}, " +
          s"Offerer queue size - ${s.offers.size}")
      } yield ()

    override def offer(a: A): F[Unit] =
      (for{
        d <- Deferred[F,Unit]
        value <- Async[F].uncancelable{
          poll => state.modify {
            case State(queue, takers, offers) if takers.nonEmpty =>
              val (taker, updatedTakers) = takers.dequeue
              (State(queue, updatedTakers, offers), taker.complete(a).void)
            case State(queue, takers, offers) if queue.size < size =>
              (State(queue.enqueue(a), takers, offers), Async[F].unit)
            case State(queue, takers, offers) =>
              val cleanup = state.update(s => s.copy(offers = s.offers.filter(_._2 ne d)))
              (State(queue, takers, offers.enqueue((a, d))), poll(d.get).onCancel(cleanup))
          }
        }
      }yield value).flatten

    override def take: F[A] =
      (for{
        d <- Deferred[F,A]
        value <- Async[F].uncancelable{
          poll => state.modify {
            case State(queue, takers, offers) if queue.nonEmpty =>
              val (elem, updated) = queue.dequeue
              if offers.nonEmpty then
                val ((elem, od), newOffers) = offers.dequeue
                (State(queue.enqueue(elem), takers, newOffers), od.complete(()).as(elem))
              else (State(updated, takers, offers), Async[F].pure(elem))
            case State(queue, takers, offers) =>
              if offers.nonEmpty then
                val ((elem, od), newOffers) = offers.dequeue
                (State(queue,takers, newOffers), od.complete(()).as(elem))
              else
                val cleanup = state.update(s => s.copy(takes = s.takes.filter(_ ne d)))
                (State(queue, takers.enqueue(d), offers), poll(d.get).onCancel(cleanup))
          }
        }
      } yield value).flatten
