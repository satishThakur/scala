package pmatch

object Patterns extends App{

  class Person(n: String, a: Int){
    val name = n
    val age = a
  }

  object Person{
    //def unapply(p : Person) : Option[String] = Some(p.name)
    def unapplySeq(p: Person): Option[Seq[Char]] =
      if p.name.isEmpty then Some(Seq.empty)
      else unapplySeq(Person(p.name.tail, 0)) map(p.name.head +: _)
  }

  val p = Person("John", 32)

  val name = p match {
    case Person(n) => n
    case _ => "unknown"
  }

  val chars = p match {
    case Person(f,s, _*) => (f,s)
    case _ => ('N', 'N')
  }
  println(chars)


}
