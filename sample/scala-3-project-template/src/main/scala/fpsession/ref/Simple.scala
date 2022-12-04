package fpsession.ref

object Simple extends App:
/**
  var i = 0

  def add(delta: Int): Int =
    i = i + delta
    i

  assert(add(2) == add(2))

**/

  def add(value: Int, delta: Int) : Int =
    value + delta

  assert(add(1,2) == add(1,2))

