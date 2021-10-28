package monad

case class Lazy[+T](v: () => T):
  def flatMap[U](f: T => Lazy[U]): Lazy[U] = f(v())

object Lazy:
  def unit[T](t: =>T): Lazy[T] =
    lazy val tt = t
    Lazy(() => tt)


object LazyMonad extends App:
  val l = Lazy.unit{
    println("hey there!!")
    42
  }
  l match {
    case Lazy(v) => println(v())
  }