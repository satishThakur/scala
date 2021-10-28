package com.satish.streams

object PouringTest {

  def main(args: Array[String]): Unit = {
    val p = new Pouring(Vector(4,9))
    val sol = p.solution(6)

    println(sol.take(1).toList)
  }

}
