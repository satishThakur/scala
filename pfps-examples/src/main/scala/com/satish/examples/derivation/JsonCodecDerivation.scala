package com.satish.examples.derivation
import io.circe.Codec
import io.circe.parser.decode
import io.circe.syntax.*

object JsonCodecDerivation extends App:
  println("HW!!")

  case class Address(streetName: String, streetNumber : Int, flat : Option[String]) derives Codec.AsObject

  val myAddress = Address("some xyz street", 13, None)

  val addressJson = myAddress.asJson.spaces2
  println(addressJson)

  val anotherAddress = decode[Address](addressJson)
  println(myAddress)
  println(anotherAddress)

