package com.satish.typeclass


trait Show[T]{
  def show(t: T): String
}

object Show{

  def apply[A](implicit show: Show[A]): Show[A] = show

  def show[A](a: A)(implicit show: Show[A]): String = Show[A].show(a)

  implicit class ShowOps[A](a: A){
    def show(implicit s: Show[A]) = s.show(a)
  }

  implicit val intshow: Show[Int] = new Show[Int] {
    override def show(t: Int): String = s"int $t"
  }
}


import Show._
object Example {

  def main(args: Array[String]): Unit = {
    println(20.show)
  }

}
