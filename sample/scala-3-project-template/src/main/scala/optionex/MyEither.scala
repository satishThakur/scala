package optionex

package chap4



//Disjoint union of two types!!
sealed trait Either[+E, +A]{
  def map[B](f: A => B): Either[E,B] = this match{
    case Left(v) => Left(v)
    case Right(v) => Right(f(v))
  }

  def flatMap[EE >:E, B](f: A => Either[EE,B]): Either[EE,B] = this match{
    case Left(v) => Left(v)
    case Right(v) => f(v)
  }

  def orElse[EE >:E, B >: A](b: =>Either[EE,B]): Either[EE,B] = this match{
    case Left(_) => b
    case Right(v) => Right(v)
  }

  def map2[EE >: E, B, C](b: Either[EE,B])(f: (A, B) => C) : Either[EE,C] = {
    this.flatMap(a => b.map(b1 => f(a,b1)))
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object App{
  def main(args: Array[String]): Unit = {
    //Either.unsafeDiv(5,0)

    println(Either.safeDiv(5,0))
  }
}


object Either {

  def unsafeDiv(x: Int, y: Int): Int = x/y

  def safeDiv(x: Int, y: Int): Either[Exception,Int] = {
    Try(x/y)
  }

  def Try[A](a: =>A): Either[Exception,A] = {
    try Right(a)
    catch{case e: Exception => Left(e)}
  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = es match{
    case Nil => Right(Nil)
    case x :: xs => sequence(xs).flatMap(ll => x.map(x1 => x1::ll))
  }

  def sequence1[E,A](es: List[Either[E,A]]): Either[E,List[A]] = {
    es.foldRight[Either[E,List[A]]](Right(Nil))( (v,el) => v.map2(el)(_ :: _) )
  }


  def traverse[E,A,B](as : List[A])(f: A => Either[E,B]): Either[E,List[B]] = {
    as.foldRight[Either[E,List[B]]](Right(Nil))( (v, el) => el.flatMap(ll => f(v).map(v1 => v1::ll)) )
  }

  def traverse2[E,A,B](as : List[A])(f: A => Either[E,B]): Either[E,List[B]] = {
    as.foldRight[Either[E,List[B]]](Right(Nil))( (v, el) => f(v).map2(el)(_ :: _) )
  }

  def sequence3[E,A](es: List[Either[E,A]]): Either[E,List[A]] = traverse(es)(x => x)

}
