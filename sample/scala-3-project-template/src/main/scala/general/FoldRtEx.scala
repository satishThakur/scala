package general

object FoldRtEx extends App{

  def foldRight[T,U](list: List[T], z: U, f: (T, =>U) => U): U = {
    println("called for" + list)
    list match {
      case Nil => z
      case x :: xs => f(x, foldRight(xs, z, f))
    }
  }

  def exists[T](list: List[T], p: T => Boolean): Boolean =
    foldRight(list, false, (a,b) => p(a) || b)
  println(exists(List(1,2,3,4,5), _ > 0))




}
