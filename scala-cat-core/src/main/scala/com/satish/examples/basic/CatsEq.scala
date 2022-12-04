package com.satish.examples.basic

object CatsEq extends App:
  //typeclass definition..
  import cats.Show
  import cats.syntax.show.*

  val showInt = Show.apply[Int]
  val str = showInt.show(234)

  println(234.show)
  println(s"Hello cats!! $str")

  case class Person(name: String, age: Int)

  //This is old Scala 2 style.
  /*
  implicit val personShow: Show[Person] = new Show[Person]:
    override def show(t: Person): String = s"${t.name} is ${t.age} years old"
  */

  given personShow: Show[Person] with
    override def show(t: Person): String = s"${t.name} is ${t.age} years old"
  val sally = new Person("Sally", 34)

  println(sally.show)
