package ctx

trait Ord[T]:
  def compare(o1: T, o2: T): Int
  def isLessThanEqual(o1: T, o2: T): Boolean = compare(o1, o2) <= 0

  extension(t: T)
    def <= (other: T): Boolean = isLessThanEqual(t, other)

object Ord:
  given intOrd: Ord[Int] with
    override def compare(o1: Int, o2: Int): Int = o1.compareTo(o2)

  given stringOrd: Ord[String] with
    override def compare(o1: String, o2: String): Int =  o1.compareTo(o2)


object Sorting:
  def isort[T](ls: List[T])(using Ord[T]): List[T] =
    if ls.isEmpty then ls else insert(ls.head, isort(ls.tail))

  def insert[T](elem: T, sorted: List[T])(using Ord[T]): List[T] = sorted match {
    case Nil => List(elem)
    case x :: xs => if elem <= x then elem :: sorted else x :: insert(elem, xs)
  }

object SortingMain extends App:
  println(Sorting.isort(List(10, 3, 50, 33)))

  println(Sorting.isort(List("Dave", "John", "Satish", "Amy", "Monk")))