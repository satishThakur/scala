package general

object ExtendFunc extends App:
  //No surprise as int => int is Syntactic sugar for Function1[Int,Int]
  class AddTen extends (Int => Int):
    override def apply(n: Int) = n + 10

  class AnotherAddTen extends Function1[Int, Int]:
    override def apply(v1: Int): Int = v1 + 10

  val a = AddTen()
  val b = AnotherAddTen()

  println(a(4))
  println(b(4))

  def hof(n: Int)(f : Int => Int): Int = f(n)

  println(hof(5)(a))
  println(hof(5)(b))


end ExtendFunc