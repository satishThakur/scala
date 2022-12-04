package com.satish.examples.intro

//this is typeClass definition
trait Printable[T]:
  def format(t: T): String

object PrintableInstances:
  given intFormatter: Printable[Int] with
    override def format(t: Int): String = s"int: ${t.toString}"
  given strFormatter: Printable[String] with
    override def format(t: String): String = s"str: $t"

  given optionFormatter[A](using pa: Printable[A]): Printable[Option[A]] with
    override def format(t: Option[A]): String = t match {
      case Some(a) => s"Some[${pa.format(a)}]"
      case None => "!None!"
    }

object Printable:
  def format[A](a: A)(using p: Printable[A]): String = p.format(a)
  def print[A](a: A)(using p: Printable[A]): Unit = println(p.format(a))

object SomeScope:
  case class Cat(name: String, age: Int, color: String)
  object Cat:
    given catPrintable: Printable[Cat] with
      override def format(t: Cat): String = s"${t.name} is ${t.age} old ${t.color} Cat."

object PrintableSyntax:
  extension[T] (t: T)(using p: Printable[T])
    def format: String = p.format(t)
    def print: Unit = println(p.format(t))


object PrintMain extends App:
  import SomeScope.Cat
  import PrintableInstances.{intFormatter, optionFormatter, strFormatter}
  val salary = 1000
  Printable.print(salary)
  val marigold = Cat("Marigold", 5, "golden")
  Printable.print(marigold)

  import PrintableSyntax.*
  marigold.print

  Option(24).print
  Option("hello").print