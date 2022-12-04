package practise
import state.*

object Listhelper:
  
  def foldLeft[A, B](ls : List[A], z: B, f: (B, A) => B) : B = ls match {
    case Nil => z
    case x :: xs => foldLeft(xs, f(z,x), f)
  }

  def zipWithIndex[A](ls : List[A]): List[(A,Int)] =
    val stf : State[Int, List[(A, Int)]] = ls.foldLeft(State.unit(List[(A, Int)]()))((s,a) => for{
      acc <- s
      st <- State.get
      _ <- State.set(st + 1)
    }yield (a, st) :: acc)
    stf.run(0)(0).reverse




object Zip extends App:
  println("hello world!")

  println(Listhelper.zipWithIndex(List(1,2,3,4,5)))
