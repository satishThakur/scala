package practise.extn.version1

//TypeClass - simple Functor typeclass
trait Functor[F[_]]:
  def map[A,B](fa: F[A])(f: A => B) : F[B]

case class Container[T](value: T)

object MyFunctors:
  given cFunc: Functor[Container] with
    override def map[A, B](fa: Container[A])(f: A => B): Container[B] = Container(f(fa.value))

object MySyntax:
  extension[A](ca: Container[A])(using F : Functor[Container])
    def map1[B](f: A => B): Container[B] = F.map(ca)(f)


object ExtnVersion1 extends App:
  println("hello world")

  //version one without extension
  def extractMappedValue[A,B](ca : Container[A])(f: A => B)(using F: Functor[Container]): B =
    F.map(ca)(f).value

  import MyFunctors.given
  println(extractMappedValue(Container(123))(_ * 2))

  import MySyntax.map1

  //variant 2 - where provided there is typeclass Instance we can augment syntax.
  def extractV2[A,B](ca : Container[A])(f: A => B): B =
    ca.map1(f).value

  println(Container(222).map1(_ * 2).value)



