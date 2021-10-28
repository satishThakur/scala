package com.satish.example

object Test {

  def main(args: Array[String]): Unit = {

    val s = "satish"

    val freq = s.groupBy(identity).map(p => (p._1, p._2.length)).toList.sorted



    println(freq)


  }

}
