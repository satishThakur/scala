package functor

trait Functor[F[_]]:
  extension[A](fa: F[A])
    def map[B](f : A => B) : F[B]


sealed trait Tree[E]

case class Node[E](l : Tree[E], r : Tree[E]) extends Tree[E]

case class Leaf[E](v: E) extends Tree[E]


object Functor:
  given optionFunctor: Functor[Option] with
    extension[A](fa:  Option[A])
      def map[B](f : A => B) : Option[B] = fa.map(f)

  given treeFunctor: Functor[Tree] with
    extension[A](fa:  Tree[A])
      def map[B](f : A => B) : Tree[B] = fa match {
        case Node(l, r) => Node(l.map(f), r.map(f))
        case Leaf(v) => Leaf(f(v))
      }



object FunctorApp extends App:
  println("Hello there..")
  import Functor.given
  val name : Option[String] = Some("ram")


  println(name.map(_.toUpperCase))

  val myTree : Tree[Int] = Node(Node(Leaf(10), Leaf(3)), Leaf(4))

  println(myTree.map(_ * 2))