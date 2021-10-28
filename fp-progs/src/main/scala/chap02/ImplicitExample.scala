package chap02

object ImplicitExample {

  def main(args: Array[String]): Unit = {
    println(max(List(6,9,3,6,14,7,2)))
  }


  def max[T](l : List[T])(implicit order : Ordering[T]) : T = l match{

    case Nil => throw new NoSuchElementException("Nil.Max")
    case x :: Nil => x
    case x :: xs =>
      val m = max(xs)
      if(order.gt(x,m)) x else m

  }

}
