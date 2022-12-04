package io
import monad.*

import scala.annotation.tailrec
import scala.io.StdIn.readLine

sealed trait IO[A]:
  def map[B](f: A => B): IO[B] = flatMap(f andThen (Return(_)) )
  def flatMap[B](f: A => IO[B]): IO[B] = new FlatMap[A,B](this, f)
  def forever[B] : IO[B] =
    this flatMap(_ => forever)

case class FlatMap[A,B](current : IO[A], cont : A => IO[B]) extends IO[B]
case class Return[A](a : A) extends IO[A]
case class Suspend[A](a : () => A) extends IO[A]

object IO extends Mon[IO]:

  @tailrec
  def run[A](io : IO[A]) : A = io match {
    case Return(x) => x
    case Suspend(f) => f()
    case FlatMap(current, next) => current match {
      case Return(a) => run(next(a))
      case Suspend(fa) => run(next(fa()))
      case FlatMap(c,n) =>  run(c flatMap(z => n(z) flatMap next))              //FlatMap(FlatMap(c,n), next)
    }
  }

  def unit[A](a: => A): IO[A] = new Suspend[A](() => a)
  def flatMap[A,B](a: IO[A])(f: A => IO[B]): IO[B] = a flatMap f

  def apply[A](a: => A): IO[A] = unit(a)

  def forever[A,B](a: IO[A]): IO[B] = a.forever

  extension[A] (io : IO[A])
    def runUnsafe: A = run(io)

object Console:
  def printLine(s: String): IO[Unit] = IO{println(s)}
  def read: IO[String] = IO{readLine}

object ExampleMain extends App:
   val converter: IO[Unit] = for{
     _ <- Console.printLine("enter a number")
     num <- Console.read
     c = Integer.parseInt(num.strip)
     - <- Console.printLine(s"Result is ${c * 4}")
   } yield ()

   //converter.runUnsafe
   val p = IO.forever(IO{println("hello there")})
    p.runUnsafe