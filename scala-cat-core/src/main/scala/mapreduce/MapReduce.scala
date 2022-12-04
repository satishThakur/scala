package mapreduce
import cats.Monoid
import cats.syntax.semigroup.*

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*

object MapReduce:

  val numcpu = 4

  def foldMap[A,B : Monoid](seq: Vector[A])(f : A => B) : B =
    seq.foldLeft(Monoid[B].empty)((b,a) => b |+| f(a))

  def parallelFoldMap[A,B : Monoid](seq: Vector[A])(f : A => B) : Future[B] =
    val batches: Vector[Vector[A]] = seq.grouped(numcpu).toVector
    val allResults : Vector[Future[B]] = batches.map(batch => Future(foldMap(batch)(f)))
    val results : Future[Vector[B]] = Future.sequence(allResults)
    results.map(b => foldMap(b)(identity))




object MapReduceApp extends App:
  import MapReduce.*
  //import cats.instances.string.*

  val concat = foldMap(Vector("he", "is", "very", "tall"))(s => s" ${s.toUpperCase} ")

  println(concat)
  val result: Future[Int] = parallelFoldMap((1 to 1000000).toVector)(identity)
  println(Await.result(result, 1.second))
