package forcomp

object Test {

  def main(args: Array[String]): Unit = {
   // println(Anagrams.dictionaryByOccurrences)

    val a = List(('a', 2), ('b', 2))
    //println(Anagrams.combinations(a))

    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))

    val x = List(('e',1), ('i',1), ('l',2), ('n',1), ('r',1), ('u',2), ('x',1), ('z',1))
    val y = List(('e',1), ('i',1), ('l',1))

    //println(Anagrams.subtract(x, y))

    //val sentence = List("Linux", "rulez")

    val sentence = List("Linux", "rulez")

    val combs = Anagrams.combinations(Anagrams.sentenceOccurrences(sentence))

    println(combs.mkString("\n"))
    println(combs.length)

    println(Anagrams.sentenceAnagrams(sentence))
  }

}
