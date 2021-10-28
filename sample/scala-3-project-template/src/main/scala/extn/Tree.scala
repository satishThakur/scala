package extn

//covariant tree..
sealed trait Tree[+T]
case class Leaf[T](value: T) extends Tree[T]
case class Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T]

object TreeExtn:
  extension[A](t: Tree[A])
    def map[B](f : A => B): Tree[B] = t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(l.map(f), r.map(f))
    }

    def forAll(predicate: A => Boolean): Boolean = t match {
      case Leaf(v) => predicate(v)
      case Branch(l, r) => l.forAll(predicate) && r.forAll(predicate)
    }

    def reduce(f : (A, A) => A): A = t match {
      case Leaf(v) => v
      case Branch(l, r) => f(l.reduce(f), r.reduce(f))
    }

object TreeMain extends App:
  import TreeExtn.*
  val tree = Branch(
    Branch(Leaf(3), Leaf(5)),
    Leaf(10)
  )

  val mappedTree = tree.map(_ * 2)

  println(tree.reduce(_ + _))
  println(mappedTree.reduce(_ + _))