package com.satish.examples.derivation

import cats.*
import cats.derived.semiauto.*
import cats.syntax.all.*

object SimpleDerivations extends App:
  println("Hey there")

  case class Person(name : String, salary: Int) derives Eq, Order, Show
  
  val p = Person("john", 100)
  println(p.show)
