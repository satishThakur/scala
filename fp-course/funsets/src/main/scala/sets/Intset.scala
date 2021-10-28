package sets


trait Set[T]{

  def contains(t : T) : Boolean

}


class Intset(f : Int => Boolean) extends Set[Int] {

  override def contains(t: Int): Boolean = f(t)

}

object Set{

  def union[T](s1 : Set[T], s2 : Set[T]) : Set[T] = e => s1.contains(e) || s2.contains(e)
}


object Test{

  def main(args: Array[String]): Unit = {

    val s = new Intset(x => x == 4)

    val t = new Intset(x => x == 5)


    val u = Set.union(s,t)

    println(u.contains(4))
    println(u.contains(5))
    println(u.contains(6))

  }
}