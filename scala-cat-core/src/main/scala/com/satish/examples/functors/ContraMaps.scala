package com.satish.examples.functors

import com.satish.examples.functors.ContraMaps.Box

object ContraMaps:
  trait Printable[A]:
    self =>
    def print(a: A): String

    def contraMap[B](f: B => A): Printable[B] = new Printable[B]:
      override def print(b: B): String = self.print(f(b))

  object Printable:
    given intPritable: Printable[Int] with
      override def print(a: Int): String = s"int: $a"
    given strPrintable: Printable[String] with
      override def print(a: String): String = s"str: $a"
    given booleanPrintable: Printable[Boolean] with
      override def print(a: Boolean): String = s"b $a"

    def format[A](a: A)(using p : Printable[A]) = p.print(a)

  final case class Box[A](value: A)
  object Box:
    given boxPrintable[A](using p: Printable[A]): Printable[Box[A]] = p.contraMap[Box[A]](_.value)
    /*
    given boxPrintable[A](using p: Printable[A]): Printable[Box[A]] with
      override def print(a: Box[A]): String = s"box: ${p.print(a.value)}"
    */

object ContraMapMain extends App:
  import ContraMaps.Printable.*
  println(format("hey there"))
  val b = Box("something")
  println(format(b))

