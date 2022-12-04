package tlambda

object Simple extends App:

  type MyList = [X] =>> List[X]

  val nums : MyList[Int] = List(1,2,3)

  println(nums)

  type StringMap = [X] =>> Map[String,X]

  val salary : StringMap[Int] = Map("Satish" -> 0, "Ram" -> 1000)

  type EitherWithError[E] = [X] =>> Either[E, X]

