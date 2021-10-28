package chap02

object CombSearch {

  def isPrime(n : Int) : Boolean  = (2 to n - 1).forall(x => n % x != 0)

  def main(args: Array[String]): Unit = {
    val n = (1 to 10).flatMap(x => (1 to x).map(y => (x,y))).filter(p => isPrime(p._1 + p._2))

    println(n)

    val x  = for {
      x <- 1 to 10
      y <- 1 to x
      if (isPrime(x + y))
    }yield (x,y)

    println(x)

    val name = "satish"

    val m : Map[Char, Int] = name.groupBy(x => x).map(kv => (kv._1, kv._2.size))

    println(m)




  }



}
