package io

object StackOverflow extends App:
  val f : (Int => Int) = (x: Int) => x

  val funcs: List[(Int => Int)] = List.fill(10000)(f)

  val composite : (Int => Int) = funcs.foldLeft(f)(_ compose _)

  composite(10)