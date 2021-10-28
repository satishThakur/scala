package implicits.old

object Examples extends App:
  println(List(1,5,8,3,4).sorted)

  case class Person(name: String, age: Int)

  implicit object PersonOrdering extends Ordering[Person]:
    override def compare(x: Person, y: Person): Int =
      if x.age != y.age then x.age.compare(y.age) else x.name.compareTo(y.name)

  println(List(Person("dave", 50), Person("john", 30)).sorted)

