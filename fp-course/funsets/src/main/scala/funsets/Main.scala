package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))

  val s = union(singletonSet(1), union(singletonSet(2), singletonSet(3)))

  println(FunSets.toString(s))

  val square = map(s, x => x * x)
  println(FunSets.toString(square))

  println(exists(square, x => x == 91))


}
