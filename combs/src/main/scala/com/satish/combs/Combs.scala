package com.satish.combs

object Combs {

  def main(args: Array[String]): Unit = {

    println(allCombs(5,5))
    println(allCombs0(5,5))
    println(allCombs1(5,5))

  }

  //functional way
  def allCombs(m : Int ,n : Int) : List[(Int, Int)] = {
    (1 until m map(x => 1 until(n) map(y => (x,y)))).flatten.toList

  }

  //applying simple rule - map(exp).flatten = flatMap
  def allCombs0(m : Int ,n : Int) : List[(Int, Int)] = {
    (1 until m flatMap (x => 1 until(n) map(y => (x,y)))).toList
  }


  def allCombs1(m : Int ,n : Int) : List[(Int, Int)] = {
    (for{
      x <- 1 until m
      y <- 1 until n
    } yield (x,y)).toList

  }

}

