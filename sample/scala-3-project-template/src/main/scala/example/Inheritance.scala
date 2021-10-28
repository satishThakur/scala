package example

object Inheritance extends App:
  def process(as: List[Int]): Int = as.foldRight(0)(_ + _)

  println(process(List(1,2,3,4)))

  val fas: List[Int => Int] = List(_ + 2, _ + 3, _ + 4)

  //println(process(fas))