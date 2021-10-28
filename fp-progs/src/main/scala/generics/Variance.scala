package generics

class Fruit {}

class Apple extends Fruit{}

class Jonathan extends Apple{}

class Orange extends Fruit{}


object Variance {

  def main(args: Array[String]): Unit = {

    val fruits = List(new Fruit, new Fruit, new Apple)

    val apples = List(new Apple, new Jonathan)

    fruitProcessor(fruits)
    fruitProcessor(apples)

    val fs : Array[Fruit] = Array(new Fruit, new Fruit, new Apple)
    val as : Array[Apple] = Array(new Apple, new Jonathan, new Apple)

    fruitProcessor(fs)
    //fruitProcessor(as)
  }

  def fruitProcessor(fruits : List[Fruit]): Unit = {
     fruits.foreach(_ => {})
  }

  def fruitProcessor(fruits: Array[Fruit]) : Unit = {
    fruits.foreach(_ => {})
  }

}
