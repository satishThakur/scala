package collections

object ListExamples {


  def main(args: Array[String]): Unit = {

    val s : List[Char] = flatmap("hello".toList)(c => List('.', c))
    println(s)


    val x = List(1,2,3)
    val y = List(11,22,33)

    //In for expression the inner generator produces faster than outer - the outer runs slower -
    val test = for{
      i <- x
      j <- y
    } yield (i, j)

    println(test)
  }

  //how flatmap would work:

  def flatmap[T](xs : List[T])( f : T => List[T]) : List[T] = xs match{
    case Nil => Nil
    case x :: xs => f(x) ::: flatmap(xs)(f)
  }

  def scalarProduct[T](xs : List[Double], ys : List[Double]) : Double = (for((x,y) <- xs zip ys)yield x * y).sum



}
