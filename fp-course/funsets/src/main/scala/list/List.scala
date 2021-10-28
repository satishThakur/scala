package list

trait List[T] {

  def isEmpty : Boolean

  def head : T

  def tail : List[T]

}


class Nil[T] extends List[T]{

  override def tail : List[T] = throw new NoSuchElementException("Nil.cons")

  override def head : T =  throw new NoSuchElementException("Nil.head")

  override def isEmpty : Boolean = true
}

class Cons[T](val head : T, val tail : List[T]) extends List[T]{

  override def isEmpty : Boolean = false

}

object Helper{

  def nth[T](list : List[T], n : Int) : T = {
    if (list.isEmpty) throw new IndexOutOfBoundsException
    else if(n == 0) list.head
    else nth(list, n -1)
  }

}


