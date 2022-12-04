package fpsession.ref



object RefTransparency extends App:

  val exp = 2 + 4

  val exp1 = println("hello world")

  def sum(l : List[Int]) : Int = l.foldLeft(0)(_ + _)

  //can we refactor?
  def biggerProgram(l : List[Int]): Int =
    val s = sum(l)
    val t = sum(l)
    s * t

  //Future - why Future is not referential transparent

  //Pure functionals are always RT

  val x = "hello "
  val y = x + "world"

  println(y)

  //Ref transparency is broken!!
  val x1 = new StringBuilder("hello ")

  val y1 = x1.append("world")

  println(y1)