package chap8

import java.util.concurrent.{ExecutorService, Executors}

import chap6.{RNG, Random, SimpleRNG, State}
import chap5.Stream
import chap7.Par
import chap7.Par.Par
import chap8.Prop.{Falsified, MaxSize, Passed, Proved, Result, TestCases}
import com.sun.net.httpserver.Authenticator.Success




case class SGen[A](g: Int => Gen[A]){

  //def apply(n: Int): Gen[A] = forSize(n)
  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] =
    SGen(n => g(n).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(n => g(n).flatMap(a => f(a).g(n)))

}

case class Gen[A](sample: State[RNG,A]){

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(a => f(a)))

  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def **[B](g : Gen[B]): Gen[(A,B)] =
    map2(g)( (_,_) )

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(s => listOfN(s))

  def unSized: SGen[A] = SGen(_ => this)
}


object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(s => Random.nonNegativeInt(s)).map(i => i % (stopExclusive - start) + start))

  def choose2(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(s =>{
    val (i,s1) = s.nextInt

    (i % (stopExclusive - start) + start,s1)
  }))


  def unit[A](a: A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(s => Random.nonNegativeInt(s)).map(n => n % 2 == 0))

  //TODO revisit this
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if(b) g1 else g2)

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    /* The probability we should pull from `g1`. */
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(Random.double).flatMap(d =>
      if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => listOfN(n,g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => if(n > 0) listOfN(n,g) else listOfN(1,g))


}

case class Prop(run: (MaxSize,TestCases, RNG) => Result){

  self =>

  def &&(p: Prop): Prop = Prop{
    (m,n,rng) => self.run(m,n,rng) match {
      case Passed  => p.run(m,n,rng)
      case Proved => p.run(m,n,rng)
      case x => x
    }
  }

  def ||(p: Prop): Prop = Prop{
    (m,n,rng) => run(m,n,rng) match {
      case Passed => Passed
      case Proved => Proved
      case _ => p.run(m,n,rng)
    }
  }

  /* This is rather simplistic - in the event of failure, we simply prepend
* the given message on a newline in front of the existing message.
*/
  def tag(msg: String) = Prop {
    (m,n,rng) => run(m,n,rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }

}

import Gen._

object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  case object Proved extends Result {
    def isFalsified = false
  }



  def apply(f: (TestCases,RNG) => Result): Prop =
    Prop { (_,n,rng) => f(n,rng) }


  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop((tc, rng) => {

    randomStream(as)(rng).zip(Stream.from(0)).take(tc).map{
      case (a,i) => try{
        if(f(a)) Passed else Falsified(a.toString,i)
      }catch{case e: Exception => Falsified(buildMsg(a,e),i)}
    }.find(_.isFalsified).getOrElse(Passed)
  })

  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"


  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  }


  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop{
    (max, n, rng) => {
      val casesPerSize = (n + (max - 1))/max
      val props:Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))

      val p = props.map(p => Prop{
        (max,_,rng) => p.run(max, casesPerSize,rng)
      }).toList.reduce(_ && _)
      p.run(max, n, rng)
    }
  }

  def check(a: => Boolean) : Prop = Prop{
    (_,_,_) => if(a) Proved else Falsified("()",0)
  }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  def equal[A](a: Par[A], b: Par[A]): Par[Boolean] =
    Par.map2(a,b)(_ == _)

  val esg: Gen[ExecutorService] = weighted(
    (choose(1,4).map(Executors.newFixedThreadPool), 0.75),
    (Gen.unit(Executors.newCachedThreadPool), 0.25)
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(esg ** g) {case s ** a => f(a)(s).get}



  def run(p : Prop,
          maxSize: Int = 100,
          testCases:Int = 100,
          rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit = {
    p.run(maxSize, testCases, rng) match{
      case Falsified(message,n) => print(s"! Falsified after $n passed tests: \n $message")
      case Passed => print(s"+ OK,passed $testCases tests.")
      case Proved => print(s"+ OK, Proved property.")
    }
  }
}

object ** {
  def unapply[A,B](p: (A,B)) = Some(p)
}


import Prop._
import Gen._

object App{
  def main(args: Array[String]): Unit = {

    val smallInts = Gen.choose(-10, 10)
    val maxProp = forAll(listOf1(smallInts))(x  => {
      val max = x.max
      !x.exists(_ > max)
    })

    //run(maxProp)

    val sortedProp = forAll(listOf1(smallInts))(x => {
      val sorted = x.sorted
      x.min == sorted.head
    })

    //run(sortedProp)

    //println(result)

    //val es = Executors.newCachedThreadPool()

    val p2 = Prop.checkPar{
      equal(
        Par.map(Par.unit(1))(_ + 1),
        Par.unit(2)
      )
    }

    run(p2)

  }
}
