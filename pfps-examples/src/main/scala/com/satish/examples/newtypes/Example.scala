package com.satish.examples.newtypes

object Example extends App:
  type NameT = Name.type
  object Name
   val x : NameT = Name
  println(x)