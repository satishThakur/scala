package monad

import testing.Exercise.Gen

trait Functor[F[_]]:
  def map[A,B](fa : F[A])(f: A => B): F[B]

  def distribute[A,B](fa: F[(A,B)]): (F[A], F[B]) =
    (map(fa)(_(0)), map(fa)(_(1)))

  def coDistribute[A,B](fa: Either[F[A], F[B]]): F[Either[A,B]] = fa match {
    case Left(value) => map(value)(a => Left(a))
    case Right(value) => map(value)(a => Right(a))
  }

object Functor:
  def listFunction = new Functor[List]:
    def map[A,B](fa : List[A])(f: A => B): List[B] = fa map f


trait Mon[F[_]] extends Functor[F]:
  def unit[A](a: => A): F[A]

  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]

  def map[A,B](fa : F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a,b)))

  def sequence[A](fa: List[F[A]]): F[List[A]] =
    fa.foldRight[F[List[A]]](unit(Nil))( (fa, fla) => map2(fa, fla)(_ :: _))

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight[F[List[B]]](unit(Nil))( (fa, fla) => map2(f(fa), fla)(_ :: _))

  //replicateM for listMonad - listMonad.replicateM(4, List(1,2,3,4))
  //List of List - it would contain input List in all positions.
  //Option - some Some(x) -> List cotaining x N times. For None empty list.
  //
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa, fb)((_, _))

  def filterM[A](fa: List[A])(f: A => F[Boolean]): F[List[A]] =
    fa.foldRight[F[List[A]]](unit(Nil))( (a, fa) => {
      flatMap(f(a))(b => if b then map(fa)(a :: _) else fa)
    })

  def compose[A,B,C](f: A => F[B])(g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def flatMapUsingCompose[A,B](fa: F[A])(f: A => F[B]): F[B] =
    compose((_:Unit) => fa)(f)(())

  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(a => a)

  def flatMapUsingJoin[A,B](fa: F[A])(f: A => F[B]): F[B] =
    join(map(fa)(f))


object Monad:
  import parallel.Par.{Par}
  import parallel.Par
  import state.State

  val listMonad = new Mon[List] {
    override def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa.flatMap(f)

    override def replicateM[A](n: Int, fa: List[A]): List[List[A]] =
      List.fill(n)(fa)
  }


  val streamMonad = new Mon[LazyList] {
    override def unit[A](a: => A): LazyList[A] = LazyList(a)
    override def flatMap[A, B](fa: LazyList[A])(f: A => LazyList[B]): LazyList[B] =
      fa.flatMap(f)
  }

  val parMonad = new Mon[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] =
      Par.flatMap(fa)(f)
  }

  var genMonad = new Mon[Gen]{
    override def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] = fa.flatMap(f)
  }

  var optionMonad = new Mon[Option]{
    override def unit[A](a: => A): Option[A] = Some(a)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  }

  def stateMonad[S] = new Mon[[X] =>> State[S,X]] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def someFunc[F[_], T](t: F[T])(using S: Mon[F]) = ???

  //val F = stateMonad[Int]

object Main extends App:
  val ll = Monad.listMonad.replicateM(10, List(1,2,3))
  println(ll)
  val ol = Monad.optionMonad.replicateM(10, Some("abc"))
  println(ol)
  
