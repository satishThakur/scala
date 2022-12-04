package applicative

trait Functor[F[_]]:
  def map[A,B](fa : F[A])(f: A => B): F[B]

trait Applicative[F[_]] extends Functor[F]:

  def unit[A](a: => A): F[A]

  def apply[A,B](ff: F[A=>B])(fa: F[A]): F[B] =
    map2(ff, fa)((ab, a) => ab(a))

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C) : F[C]

  def map2UsingApply[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C) : F[C] =
    apply(apply(unit(f.curried))(fa))(fb)

  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(map(fa)(f.curried))(fb))(fc)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a,_) => f(a))

  def mapUsingApply[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def traverse[A,B](ls: List[A])(f: A => F[B]): F[List[B]] =
    ls.foldRight(unit(Nil))((a,ls) => map2(f(a), ls)(_ :: _))

  def sequence[A](ls: List[F[A]]): F[List[A]] = traverse(ls)(identity)

  def replicateM[A](n : Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa, fb)( (_,_))

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
    ofa.foldLeft(unit(Map.empty)){
      case (acc, (k, fv)) => map2(acc, fv)((m,v) => m + (k -> v))
    }


trait Monad[F[_]] extends Applicative[F]:
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a,b)))

  def compose[A,B,C](fa: A => F[B])(fb: B => F[C]): A => F[C] =
    a => flatMap(fa(a))(b => fb(b))

val streamApplicative = new Applicative[LazyList] {
  override def unit[A](a: => A): LazyList[A] = LazyList.continually(a)
  override def map2[A, B, C](fa: LazyList[A], fb: LazyList[B])(f: (A, B) => C): LazyList[C] =
    fa zip fb map f.tupled
}

given eitherMonad[E]: Monad[Either[E, _]] with
//given eitherMonad[E]: Monad[[X] =>> Either[E,X]] with
  override def unit[A](a: => A): Either[E, A] = Right(a)
  override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa.flatMap(f)

trait Traverse[F[_]]:
  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] = ???


object ApplicativeApp extends App:
  println("hello there")


  def someFunc(x : Either[String,Int])(using a: Monad[Either[String,_]]) =
    a.flatMap(x)(n => Right(n + 1))

  println(someFunc(Right(5)))