package com.satish.examples.optics
import com.satish.examples.optics.IsoExamples.Person
import monocle.Iso

object IsoExamples extends App:
  println("hey there!!")

  case class Person(name: String, salary: Int)
  given person : Iso[Person, (String, Int)] = Iso[Person, (String, Int)](p => (p.name, p.salary)) {
    case (name, salary) => Person(name, salary)
  }

  def something[A,B](a : A)(using I : Iso[A,B]) : B = I.get(a)

  println(something[Person, (String, Int)](Person("jay", 100)))