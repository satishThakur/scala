package com.satish.examples.monnadtx

object Example extends App:

  val x = Option(4)

  x.fold("100")(_.toString)
