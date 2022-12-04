package com.satish.examples.monoid

object SuperAdder:
  import cats.Monoid
  import cats.syntax.monoid.*

  def addGeneric[T](l : List[T])(using monoid: Monoid[T]) : T = l.foldLeft(monoid.empty)(_ |+| _)

  def add(nums: List[Int]): Int = nums.foldLeft(Monoid.apply[Int].empty)(_ |+| _)

  def add(nums: List[Option[Int]]): Option[Int] = nums.foldLeft(Monoid.apply[Option[Int]].empty)(_ |+| _)

object SuperAdderMain extends App:
  import SuperAdder.add
  println(add( (1 to 10).toList ))

  val options = (1 to 10).toList map(x => Option(x))
  println(add(options))