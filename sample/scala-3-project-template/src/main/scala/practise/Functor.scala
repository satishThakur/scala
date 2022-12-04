package practise

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
