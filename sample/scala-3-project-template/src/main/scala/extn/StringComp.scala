package extn

object StringComp extends App:
  extension (s: String)
    def singleSpace: String =
      s.trim.split("\\s+").mkString(" ")

  val s1 = "  something    or the other  ...    "
  val s2 = "  something or      the     other ...  "
  println(s1.singleSpace)
  println(s2.singleSpace)
  println(s1.singleSpace == s2.singleSpace)