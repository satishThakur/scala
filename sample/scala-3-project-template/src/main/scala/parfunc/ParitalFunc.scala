package parfunc

object ParitalFunc extends App {

  def hof (n: Int)(f: Int => Int): Int = f(n)

  val compFunc: (Int => Int) = {
    case 1 => 100
  }

  val partialFunc: PartialFunction[Int, Int] = {
    case 1 => 100
  }



  println(compFunc(12))

  hof(1)(compFunc)
  hof(1)(partialFunc)


}
