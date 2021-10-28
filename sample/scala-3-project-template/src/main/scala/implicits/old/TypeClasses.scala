package implicits.old

object TypeClasses extends App:
  trait Equal[T]:
    def apply(o1: T, o2: T): Boolean
  object Equal:
    def apply[T](o1: T, o2: T)(implicit equal: Equal[T]): Boolean = equal(o1,o2)

  case class User(name: String, age: Int)


  implicit object UserEqualizer extends Equal[User]:
    override def apply(o1: User, o2: User): Boolean = o1.name == o2.name && o1.age == o2.age

  implicit class EqualizerOps[T](value: T):
    def customEq(another: T)(implicit eq: Equal[T]): Boolean = eq(value, another)


  def doSomething[T](first: T, second: T)(implicit equal: Equal[T]) =
    println(equal(first, second))

  val john = User("john", 20)
  val fakeJohn = User("john", 20)
  doSomething(john, fakeJohn)
  println(Equal(john, fakeJohn))
  println(john.customEq(fakeJohn))
  scala.collection.JavaConverters
