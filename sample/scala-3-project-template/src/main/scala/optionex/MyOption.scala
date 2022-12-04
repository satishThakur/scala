package optionex

package chap04


sealed trait MyOption[+T]{

  def unit[T](a: => T): MyOption[T] = MSome(a)

  def map[U](f: T => U): MyOption[U] = this match {
    case MNone => MNone
    case MSome(x) => MSome(f(x))
  }

  def flatMap[U](f: T => MyOption[U]): MyOption[U] = this match{
    case MNone => MNone
    case MSome(x) => f(x)
  }

  def getOrElse[U >: T](default: => U): U = this match{
    case MNone => default
    case MSome(x) => x
  }

  def orElse[U >: T](default: => MyOption[U]): MyOption[U] = this match{
    case MNone => default
    case x => x
  }

  def filter(f: T => Boolean): MyOption[T] = this match{
    case MNone => MNone
    case MSome(x) => if(f(x)) MNone else this
  }
}

case class MSome[T](get : T) extends MyOption[T]

case object MNone extends MyOption[Nothing]