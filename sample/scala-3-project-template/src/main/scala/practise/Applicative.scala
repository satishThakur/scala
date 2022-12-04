package practise

import monoid.Monoid

trait Applicative[F[_]] extends Functor[F] :
  self =>
  def unit[A](a: => A): F[A]

  def apply[A,B](fab: F[A => B])(fa: F[A]) : F[B] =
    fab.map2(fa)(_(_))

  extension[A](fa: F[A])
    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      apply(apply(unit(f.curried))(fa))(fb)

    def map3[B,C,D](fb: F[B], fc: F[C])(f: (A,B,C) => D) : F[D] =
      apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

    def map[B](f: A => B): F[B] =
      fa.map2(unit(()))((a, _) => f(a))

    def mapUsingApply[B](f: A => B): F[B] =
      apply(unit(f))(fa)

    def product[B](fb: F[B]) : F[(A,B)] = fa.map2(fb)((_,_))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldRight(unit(List[A]()))((fa, ls) => fa.map2(ls)(_ :: _))

  def traverse[A,B](fas: List[A])(f: A => F[B]) : F[List[B]] =
    fas.foldRight(unit(List[B]()))((fa, ls) => f(fa).map2(ls)(_ :: _))

  def replicateM[A](n : Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def product[G[_]](GA: Applicative[G]) : Applicative[[x] =>> (F[x], G[x])] = new:

    override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), GA.unit(a))

    override def apply[A, B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) : (F[B], G[B]) =
      (self.apply(fs(0))(p(0)), GA.apply(fs(1))(p(1)))

  def compose[G[_]](GA: Applicative[G]): Applicative[[x] =>> F[G[x]]] = new:
    override def unit[A](a: => A): F[G[A]] = self.unit(GA.unit(a))
    extension[A](fga: F[G[A]])
      override def map2[B, C](fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fga)(fgb)(GA.map2(_)(_)(f))


object Applicative:
  type Const[M,B] = M

  given monoidApplicative[M](using m : Monoid[M]) : Applicative[Const[M,_]] with
    override def unit[A](a: => A): Const[M, A] = m.zero
    //def apply[A,B](fab: F[A => B])(fa: F[A]) : F[B] =
    //Const[M, A => B] which itself is M
    override def apply[A, B](m1: M)(m2: M): M = m.op(m1, m2)


object ApplicativeApp extends App:
  //val l = LazyList(1,2,3,4)

  println("Applicative..")