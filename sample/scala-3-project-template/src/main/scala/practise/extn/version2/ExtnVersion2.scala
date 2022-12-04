package practise.extn.version2

/**
 * 
 * In this example we can see that having extension part of the TypeClass has benifits:
 * As the typeclass instance already adds the extension methods we do not need to have older way to add syntax.
 * Other advantage is same name - as we saw in version 1 we could not use map as extension as it was part of typeclass.
 */
trait MFunctor[F[_]]:
  extension[A](fa: F[A])
    def map[B](f: A => B) : F[B]

case class MyContainer[T](value: T)

object MyInstances:
  given mcFunc: MFunctor[MyContainer] with
    extension[A](fa: MyContainer[A])
      def map[B](f: A => B): MyContainer[B] = MyContainer(f(fa.value))

object ExtnVersion2 extends App:
  println("Hello world v2")

  val salary = MyContainer(200)

  import MyInstances.given
  print(salary.map(_ * 3))
