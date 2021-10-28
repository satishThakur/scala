package testing
import testing.Exercise.{Gen, Prop}
import state.*

object Exercise:
  import Prop.*
  case class Prop(run: (MaxSize,TestCases,RNG) => Result):
    def &&(other: Prop): Prop = Prop {
      (max, n, rng) => {
        run(max, n,rng) match {
          case Passed | Proved => other.run(max, n, rng)
          case x => x
        }
      }
    }

    def ||(other: Prop): Prop = Prop {
      (max, n, rng) => {
        run(max, n,rng) match {
          case Falsified(_, _) => other.run(max, n, rng)
          case x => x
        }
      }
    }

  object Prop:
    type FailedCase = String
    type TestCases = Int
    type SuccessCount = Int
    type MaxSize = Int

    sealed trait Result:
      def isFalsified: Boolean

    case object Passed extends Result:
        def isFalsified = false

    case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result:
      def isFalsified = true

    case object Proved extends Result:
      def isFalsified = false

    def forAll[A](g: Gen[A])(predicate: A => Boolean): Prop = Prop {
        (n, rng) => {
          lazyStream(rng, g).zip(LazyList.from(0)).take(n).map {
            case (a, i) => try {
              val passed = predicate(a)
              if passed then Passed else Falsified(a.toString, i)
            } catch {
              case e: Exception => Falsified(makeString(a, e), i)
            }
          }.find(_.isFalsified).getOrElse(Passed)
        }
      }

    //This apply is used in forAll above. Other way we could have ignored max in forAll.
    //This is like another construtor which ignored max.
    def apply(f: (TestCases,RNG) => Result): Prop =
      Prop((max, n, rng) => f(n, rng))

    def forAll[A](g: SGen[A])(predicate: A => Boolean): Prop =
      forAll(g(_))(predicate)

    def forAll[A](g: Int => Gen[A])(predicate: A => Boolean): Prop = Prop{
      (max, n, rng) => {
        val testCasesPerBin: Int = (n - 1)/ (max + 1)
        val propStream: LazyList[Prop] = LazyList.from(0).take(max min n).map(sz => forAll(g(sz))(predicate))
        //but these prop should ignore the test cases and only execute testCasesPerBin.
        val prop = propStream.map(p => Prop{
          (max, _, rng) => p.run(max, testCasesPerBin, rng) //ignore the size!!
        }).toList.reduce(_ && _)

        prop.run(max, n, rng)
      }
    }

    def run(p: Prop,
            maxSize: Int = 100,
            testCases: Int = 100,
            rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
      p.run(maxSize, testCases, rng) match {
        case Falsified(msg, n) =>
          println(s"! Falsified after $n passed tests:\n $msg")
        case Passed =>
          println(s"+ OK, passed $testCases tests.")
        case Proved =>
          println(s"+ OK, proved property.")
      }


  def lazyStream[A](rng: RNG,g: Gen[A]): LazyList[A] = {
    LazyList.unfold(rng)(rng => Some(g.sample.run(rng)))
  }

  def makeString[A](s : A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"



  case class Gen[+A](sample : State[RNG, A]):

    def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

    def flatMap[B](f: A => Gen[B]): Gen[B] =
      Gen(sample.flatMap(a => f(a).sample))
    /* A method alias for the function we wrote earlier. */
    def listOfN(size: Int): Gen[List[A]] =
      Gen.listOfN(size, this)

    /* A version of `listOfN` that generates the size to use dynamically. */
    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap(listOfN(_))

    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
      Gen.bool.flatMap(b => if b then g1 else g2)

    def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] =
      val threshold = g1(1) / ( g1(1) + g2(1))
      Gen(State(RNG.double)).flatMap(d => if d < threshold then g1(0) else g2(0))

    //dont care about size
    def unsized: SGen[A] = SGen(_ => this)


  object Gen:
    def choose(start: Int, end: Int): Gen[Int] =
      Gen(State(RNG.nonNegativeInt).map(n => start + n % (end-start)))

    def unit[A](a: => A): Gen[A] = Gen(State(rnd => (a, rnd)))

    def bool: Gen[Boolean] = Gen(State(RNG.bool))

    //we could use sequence here as state need to pass!!
    def listOfN[A](n : Int, g : Gen[A]): Gen[List[A]] =
      Gen(State.sequence(List.fill(n)(g.sample)))

    def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n))

    def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(1 max n))



  case class SGen[+A](g: Int => Gen[A]):

    def apply(n : Int): Gen[A] = g(n)

    def map[B](f: A => B): SGen[B] =
      SGen(n => g(n).map(f)) //SGen(g(_) map f)

    def flatMap[B](f: A => SGen[B]): SGen[B] =
      SGen(n => {
        g(n).flatMap(a => f(a)(n))
      })

object Main extends App:
  import Exercise.*
  println("running...")

  val small = Gen.choose(0, 20)

  val p = Prop.forAll(Gen.listOf1(small)){
    l => {
      //println(l.length)
      val max = l.max
     !l.exists(_ > max)
    }
  }
  Prop.run(p, maxSize = 5)
