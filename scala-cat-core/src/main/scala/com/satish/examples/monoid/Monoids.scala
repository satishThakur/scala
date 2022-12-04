package com.satish.examples.monoid

object MonoidsMain extends App:
  import cats.Monoid
  import cats.syntax.monoid.*
  def reduceFold[T](l : List[T])(using m: Monoid[T]): T = l.foldLeft(m.empty)(_ |+| _)

