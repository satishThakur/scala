package com.satish.examples.foldable
import cats.Foldable
import cats.instances.string.*
import cats.syntax.foldable.*

object Example extends App:
  val str = List(1,2,3).foldLeft("nil")((acc, n) => s"$n then $acc")

  println(str)

  val str1 = List(1,2,3).foldRight("nil")((n, acc) => s"$n then $acc")

  println(str1)

  val nstr = List(1,2,3).foldMap(_.toString)

  println(nstr)