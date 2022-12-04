package practise

import monoid.Foldable
import monoid.Monoid
import Applicative.Const
import state.*

//just a wraper on a type
type ID[A] = A

object ID:
  given idMoand : Monad[ID] with
    override def unit[A](a: => A): ID[A] = a
    extension[A](ia: ID[A])
      override def flatMap[B](f: A => ID[B]): ID[B] = f(ia)


trait Traverse[F[_]] extends Functor[F] with Foldable[F]:
  import Monad.given

  extension[A](fa: F[A])
    def traverse[G[_] : Applicative, B](f: A => G[B]): G[F[B]] =
      sequence(fa.map(f))
    override def map[B](f: A => B): F[B] =
      fa.traverse[ID, B](f)(using ID.idMoand)

    override def foldMap[M: Monoid](f: A => M): M =
      traverse[Const[M,_], Nothing](f)

    def zipWithSequence_ : F[(A,Int)] =
      val stf : State[Int, F[(A,Int)]] = fa.traverse(a => for{
        c <- State.get[Int]
        _ <- State.set(c + 1)
      }yield (a,c))
      stf.run(0)(0)

    def toList_ : List[A] =
      val stf : State[List[A], F[Unit]] = fa.traverse(a => for{
        l <- State.get[List[A]]
        _ <- State.set(a :: l)
      }yield ())
      stf.run(List[A]())(1).reverse

  extension[G[_] : Applicative,A](gfa: F[G[A]])
    def sequence : G[F[A]] = traverse(gfa)(identity)


object Traverse:

  given listTravere: Traverse[List] with
    extension[A](fa: List[A])
      override def traverse[G[_] : Applicative, B](f: A => G[B]): G[List[B]] =
        val GApp : Applicative[G] = summon[Applicative[G]]
        fa.foldRight(GApp.unit(List[B]()))((a, gl) => f(a).map2(gl)(_ :: _))

      override def map[B](f: A => B): List[B] = fa map f

