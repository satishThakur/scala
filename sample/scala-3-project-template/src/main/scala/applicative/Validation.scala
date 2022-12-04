package applicative

sealed trait Validation[+E, +A]

case class Success[A](a: A) extends Validation[Nothing, A]

case class Failure[E](head: E, tail: Vector[E]) extends Validation[E, Nothing]


given validApplicative[E]: Applicative[[X] =>> Validation[E,X]] with
  def unit[A](a: => A): Validation[E,A] = Success(a)
  def map2[A,B,C](fa: Validation[E,A], fb: Validation[E,B])(f: (A,B) => C): Validation[E,C] = (fa, fb) match {
    case (Success(a), Success(b)) => Success(f(a,b))
    case (Failure(h,t), Failure(h1, t1)) => Failure(h, t ++ Vector(h1) ++ t1)
    case (_, e@Failure(_,_)) => e
    case (e@Failure(_,_), _) => e
  }