package chap04


sealed trait Option[+T]{

  def map[U](f: T => U): Option[U] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def flatMap[U](f: T => Option[U]): Option[U] = this match{
    case None => None
    case Some(x) => f(x)
  }

  def getOrElse[U >: T](default: => U): U = this match{
    case None => default
    case Some(x) => x
  }

  def orElse[U >: T](default: => Option[U]): Option[U] = this match{
    case None => default
    case x => x
  }

  def filter(f: T => Boolean): Option[T] = this match{
    case None => None
    case Some(x) => if(f(x)) None else this
  }
}

case class Some[T](get : T) extends Option[T]

case object None extends Option[Nothing]