package com.satish.examples.monads

object OptionM extends App:

  def getInt(s: String): Option[Int] = s.toIntOption

  def divide(n : Int, d: Int): Option[Int] = if d == 0 then None else Some(n / d)

  def strDiv(n: String, d: String): Option[Int] = for{
    num <- getInt(n)
    denom <- getInt(d)
    div <- divide(num, denom)
  } yield div

  def strDiv1(n: String, d: String): Option[Int] =
    getInt(n).flatMap(
      num => getInt(d).
        flatMap( denom => divide(num, denom))
    )

  def doubleDiv(n: String, d: String): Option[Int] = for{
    num <- getInt(n) map (_ * 2)
    denom <- getInt(d) map (_ - 5)
    div <- divide(num, denom)
  } yield div * 3

  def doubleDiv1(n: String, d: String): Option[Int] =
    getInt(n).map(_ * 2).flatMap(
      num => getInt(d).map(_ -5).flatMap(
        denom => divide(num, denom).map(_ * 3)
      )
    )


  println(strDiv("100", "10"))
  println(strDiv("100", "somthing"))