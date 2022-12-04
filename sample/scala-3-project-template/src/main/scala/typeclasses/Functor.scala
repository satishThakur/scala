package typeclasses

trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

case class Wrapper[+A](get: A):
  def map[B](f : A => B) : Wrapper[B] = Wrapper(f(get))


trait Functor[F[_]]:
  self =>
  //def map[A,B](fa : F[A])(f: A => B): F[B]
  extension[A] (fa: F[A])
    def map[B](f: A => B): F[B]


object Functor:

  given listFunctor: Functor[List] with
    extension[A] (list : List[A])
      override def map[B](f: A => B): List[B] = list map f

  given treeFunctor: Functor[Tree] with
    extension[A] (tree : Tree[A])
      override def map[B](f: A => B): Tree[B] = tree match {
        case Leaf(v) => Leaf(f(v))
        case Branch(l, r) => Branch(l.map(f), r.map(f))
      }
/*
object FunctorExtns:
  extension[F[+_], A](container: F[A])(using functor: Functor[F])
    def map[B](f: A => B): F[B] = functor.map(container)(f)
*/
object SomeApi:
  def multiply[F[_] : Functor](container: F[Int]) : F[Int] =
    container.map(_ * 2)

object FunctorMain extends App:
  import SomeApi.multiply


  //import FunctorExtns.*
  println(multiply(List(3,5,7,9)))
  import Functor.listFunctor
  List(1,2,3).map(_ * 2)

  import Functor.given

  val myTree : Tree[Int] = Branch(Branch(Leaf(10), Leaf(15)), Leaf(4))
  println(myTree.map(_ * 3))

  given wrapperFunctor : Functor[Wrapper] with
    extension[A] (w : Wrapper[A])
      override def map[B](f: A => B): Wrapper[B] = w map f

  multiply[Wrapper](Wrapper(10))
