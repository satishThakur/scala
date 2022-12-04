package typeclasses

class ListFunctor extends Functor[List]{
  extension[A] (list : List[A])
    override def map[B](f: A => B): List[B] = list map f
}

object Api:
  def bytwo[F[_] : Functor](container: F[Int]) : F[Int] =
    container.map(_ * 2)

object ListFunctorApp extends App:
  //given listF : Functor[List] = new ListFunctor
  //println(Api.bytwo(List(1,2,3))(using new ListFunctor))
  given listF : Functor[List] = new ListFunctor
  println(Api.bytwo(List(1,2,3)))

