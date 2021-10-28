package monad

trait Applicative[F[_]] extends Functor[F]:
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C]




object ApplicativeMain extends App
