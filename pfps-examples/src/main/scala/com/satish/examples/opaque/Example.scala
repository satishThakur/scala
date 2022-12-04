package com.satish.examples.opaque


object Data:
  opaque type Name = String
  def Name(s: String): Name = s
  extension (n : Name)
    def value: String = n


object Example extends App:
  import Data.Name

  val name: Name = Data.Name("satish")

  println(name.value)

