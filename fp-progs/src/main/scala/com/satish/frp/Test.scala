package com.satish.frp

object Test {

  def main(args: Array[String]): Unit = {

    val x = Var(5)

    (0 until 100).foreach( _ => addFour(x))

    println(x.numObs())
  }

  def addFour(s : Signal[Int]): Signal[Int] = Signal(s() + 4)

}
