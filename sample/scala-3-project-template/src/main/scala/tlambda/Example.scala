package tlambda

import tlambda.Example.Wrapper

import scala.collection.mutable
import scala.util.Try

object Example:

  trait Wrapper[F[_]]:
    def value[A](fa: F[A]): A

  def rtEither[B] = new Wrapper[[x] =>> Either[B,x]]{
    override def value[A](fa: Either[B,A]) = fa match {
      case Left(_) => throw Exception("")
      case Right(x) => x
    }
  }


object Main extends App:
  import Example.*
  println("hello")




