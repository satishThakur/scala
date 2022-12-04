package fpsession.sm

object Sm extends App:

  def square(x : Int): Int = x * x

  def sumSquare(a: Int, b: Int): Int =
    square(a) + square(b)

  val a = 4 + 5
  val b = 3 + 4
  val r = sumSquare(a, b)

  //sumSquare(9, 7) ...

  //square(9) + sqaure(7)

  // (9 * 9) + (7 * 7)

  // 81 + 49 -->

