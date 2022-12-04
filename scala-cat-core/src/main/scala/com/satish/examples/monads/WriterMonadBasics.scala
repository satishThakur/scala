package com.satish.examples.monads

import cats.data.Writer
import cats.syntax.applicative.*
import cats.syntax.writer.*
import cats.instances.vector.*
object WriterMonadBasics extends App:
  //create a writer with no logs.

  type LogWriter[A] = Writer[Vector[String], A]

  val l1 = 123.pure[LogWriter]

  val l  = Writer(Vector("hello"), 25)

  val ll = Vector("some", "data").tell
  //Writer.tell(Vector(""))

  val fl = ll.flatMap(_ => l)

  println(fl.run)

  val ff = for{
    i <- l1
    j <- l
  } yield (i + j)

  println(ff.run)

