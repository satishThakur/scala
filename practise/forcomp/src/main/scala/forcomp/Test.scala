package forcomp

object Test {

  def main(args: Array[String]): Unit = {
    val sentence = List("Linux", "ralez")
    //val sentence = List("yes", "man")

    //val combs = Anagrams.combinations(Anagrams.sentenceOccurrences(sentence))

    //println(combs.mkString("\n"))
    //println(combs.length)

    //println(Anagrams.sentenceAnagrams(sentence))

    //println(Anagrams.combinations(List(('e',1), ('i',1), ('l',2), ('n',1), ('r',1), ('u',2), ('x',1), ('z',1))))

    //println(Anagrams.combinations(List(('i', 1))))

    Anagrams.sentenceAnagrams(List("i"))

  }




}
