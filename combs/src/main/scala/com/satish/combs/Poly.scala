package com.satish.combs

class Poly(c : Map[Int, Double]) {

  val p = c.withDefaultValue(0.0)

  override def toString: String = c.toList.sorted.reverse.map(p => " " + p._2 + "x^" + p._1).mkString("+")
  //this.c.foldLeft("")((s,p) => s + " " + p._2 + "x^" + p._1 + "+")

  def add(other : Poly) : Poly = new Poly(p.foldLeft(other.p)(combine))

  def combine(m : Map[Int,Double], p : (Int,Double)) : Map[Int,Double] = {
    val adjusted  = (p._1, p._2 + m(p._1))
    m + adjusted
  }

}

object App{

  def main(args: Array[String]): Unit = {
    val p = new Poly(Map(0 -> 2, 1 ->3, 2->5))
    val r = new Poly(Map(0 -> 5, 2 -> 3, 5 -> 8))
    println(p)
    println(p.add(r))
  }

}
