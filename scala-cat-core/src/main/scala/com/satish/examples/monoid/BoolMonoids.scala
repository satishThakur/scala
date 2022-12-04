package com.satish.examples.monoid

object BoolMonoids extends App:
  import cats.Monoid

  given andMonoid: Monoid[Boolean] with
    override def empty: Boolean = true
    override def combine(x: Boolean, y: Boolean): Boolean = x && y

  given listMonoid[A]: Monoid[List[A]] with
    override def empty: List[A] = List.empty
    override def combine(ls: List[A], lx: List[A]) : List[A] = ls ++ lx


  val status = List(true, false, true, true)

  val combined = status.foldLeft(andMonoid.empty)(andMonoid.combine(_, _))

