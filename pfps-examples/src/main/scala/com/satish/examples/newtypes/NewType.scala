package com.satish.examples.newtypes
import cats.{Eq, Show, Order}
import io.circe.{Encoder, Decoder}
import monocle.Iso

import cats.syntax.all.*


trait Wrapper[T,A]:
  def iso: Iso[T,A]

object Wrapper:
  //is this any different than summon - or just convinicence?
  //transparent inline def summon[T](using inline x: T): x.type = x
  def apply[T,A](using wr : Wrapper[T,A]) : Wrapper[T,A] = wr


abstract class NewType[A](using
                          eq : Eq[A],
                          show: Show[A],
                          ord: Order[A],
                          enc: Encoder[A],
                          dec: Decoder[A] ):

  opaque type Type = A
  extension(t : Type) inline def value: A = t

  inline def apply(a: A) : Type = a

  given wr : Wrapper[A, Type] = new Wrapper[A, Type]{
    override def iso : Iso[A, Type] = Iso[A, Type](apply(_))(_.value)
  }

  given egt: Eq[Type] = eq
  given showt: Show[Type] = show
  given ordt: Order[Type] = ord
  given enct: Encoder[Type] = enc
  given dect: Decoder[Type] = dec


object MyMain extends App:
  type Name = Name.Type
  object Name extends NewType[String]

  val satish : Name = Name("satish")

  val wrapper : Wrapper[String,Name] = Wrapper[String,Name]

  val n: Name = wrapper.iso.get("something")
  println(n)


  println(satish.show)