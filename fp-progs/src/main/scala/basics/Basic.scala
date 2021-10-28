package basics

object Basic {

  def main(args: Array[String]): Unit = {

    val l = List(1 -> "One", 2 -> "two", 3 -> "three")

    println(l.map{case (n,s) => (n,s.length)})

    val width = 22
    val numTasks = 7
    val interval = width / numTasks
    val x = (0 until width by interval).toList.appended(width)

    val points = x.zip(x.tail)

    println(x)
    println(points)


  }

}
