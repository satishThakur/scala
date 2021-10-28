package chap02

sealed trait MyList[+T]{

}

case object XNil extends MyList[Nothing]

case class XCons[T](head : T, tail : MyList[T]) extends MyList[T]


object MyList{

  def apply[T](vals : T*) : MyList[T] = {
    if (vals.isEmpty) XNil
    else XCons(vals.head, apply(vals.tail : _*))

  }
}


object Test{

  def main(args: Array[String]): Unit = {

    //3.1
    val y = MyList(1, 2, 3, 4, 5) match {
      case XCons(x, XCons(2, XCons(4, _))) => x
      case XNil => 42
      case XCons(x, XCons(y, XCons(3, XCons(4, _)))) => x + y
      case _ => 101
    }

    println(y)

    /*
    val x = List(1,2,3,4,5) match {   case Cons(x, Cons(2, Cons(4, _))) => x   case Nil => 42   case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y   case Cons(h, t) => h + sum(t)   case _ => 101 }

Paul Chiusano Rúnar Bjarnason. Functional Programming in Scala (Kindle Locations 1055-1060). Manning Publications. Kindle Edition.
     */
  }

}



