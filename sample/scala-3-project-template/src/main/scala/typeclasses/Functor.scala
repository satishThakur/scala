package typeclasses

trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

trait Functor[F[_]]:
  def map[A,B](fa : F[A])(f: A => B): F[B]

object Functor:
  given listFunctor: Functor[List] with
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa map f

  given treeFunctor: Functor[Tree] with
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

object FunctorExtns:
  extension[F[+_], A](container: F[A])(using functor: Functor[F])
    def map[B](f: A => B): F[B] = functor.map(container)(f)

object SomeApi:
  def multiply[F[_]](container: F[Int])(using functor: Functor[F]): F[Int] =
    functor.map(container)(_ * 2)


object FunctorMain extends App:
  import SomeApi.multiply
  import FunctorExtns.*
  println(multiply(List(3,5,7,9)))

  val myTree = Branch(Branch(Leaf(10), Leaf(15)), Leaf(4))
  println(myTree.map(_ * 3))
