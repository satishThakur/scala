package implicits.old

object ImplicitClass extends App:
  case class Person(name: String):
    def greet: String = s"hello $name"

  implicit class StringPerson(s: String):
    def greet: String = new Person(s).greet

  println("satish".greet)
