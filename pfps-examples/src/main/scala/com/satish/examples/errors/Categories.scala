package com.satish.examples.errors
import cats.ApplicativeThrow
import cats.MonadThrow
import cats.effect.std.Random
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.functor.*
import cats.syntax.flatMap.*
//import cats.implicits.*
//import cats.*
//import cats.effect.*

case class Category(name: String)

//Category Algebra
trait Categories[F[_]]:
  def findAll: F[List[Category]]


object Categories:
  def make[F[_] : Random : MonadThrow] : Categories[F] =
    val APP : MonadThrow[F] = summon[MonadThrow[F]]
    new Categories[F] {
    override def findAll: F[List[Category]] = Random[F].nextInt.flatMap{
      case i if i % 2 == 0 => APP.pure(List(Category("Vegetable")))
      case _ => APP.raiseError(new RuntimeException("something wrong"))
    }
  }


object MyApp extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    val randEffect = Random.scalaUtilRandom[IO]
    val categories : IO[List[Category]] = randEffect.flatMap( f => {
      given r : Random[IO] = f
      val c = Categories.make[IO]
      c.findAll
    })

    (for{
      c <- categories
      _ <- IO.println(c)
    } yield ()).as(ExitCode.Success)





