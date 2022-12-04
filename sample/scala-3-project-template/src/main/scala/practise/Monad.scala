package practise
import state.*

trait Monad[F[_]] extends Applicative[F] :
  extension[A] (fa: F[A])
    def flatMap[B](f: A => F[B]): F[B]

    //Monads are natually Functors!!
    override def map[B](f: A => B): F[B] = fa.flatMap(a => unit(f(a)))

    //Monads are Applicative by default.
    // But map2 is implemented in terms of flatMap which has implications.
    override def map2[B,C](fb: F[B])(f: (A,B) => C): F[C] =
      fa.flatMap(a => fb.map(b => f(a,b)))


object Monad:
  //brute force solution
  type EitherStringOr[T] = Either[String,T]
  given eitherStringOrMonad : Monad[EitherStringOr] with
    override def unit[A](a: => A): EitherStringOr[A] = Right(a)
    extension[A](fa: EitherStringOr[A])
      def flatMap[B](f: A => EitherStringOr[B]) : EitherStringOr[B] = fa match {
        case Right(a) => f(a)
        case Left(e) => Left(e)
      }
  //Problem with above is - we would have to define monad for every error type for Either

  //here comes type lambda to rescue
  // [X] =>> Either[E,X] means a type with takes one parameter and produces Either - so 
  // this is again a type constructor but more like currying.
  given eitherMonadWithTL[E] : Monad[[X] =>> Either[E,X]] with
    override def unit[A](a: => A): Either[E, A] = Right(a)
    extension[A](fa: Either[E,A])
      def flatMap[B](f: A => Either[E,B]) : Either[E,B] = fa match {
        case Right(a) => f(a)
        case Left(e) => Left(e)
      }

  given eitherMonad[E]: Monad[Either[E,_]] with
    override def unit[A](a: => A): Either[E, A] = Right(a)
    extension[A](fa: Either[E,A])
      def flatMap[B](f: A => Either[E,B]) : Either[E,B] = fa match {
        case Right(a) => f(a)
        case Left(e) => Left(e)
      }

  given stateMonad[S]: Monad[State[S,_]] with
    override def unit[A](a: => A): State[S, A] = State.unit(a)
    extension[A](fa: State[S,A])
      def flatMap[B](f: A => State[S,B]) : State[S,B] = fa flatMap f