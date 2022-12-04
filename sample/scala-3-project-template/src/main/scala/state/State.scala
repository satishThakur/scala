package state

trait RNG:
  def nextInt: (Int, RNG)

object RNG:
  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
  end Simple

  def bool(r : RNG): (Boolean, RNG) =
    val (i, r1) = r.nextInt
    (i %2 ==0, r1)

  //Problem -->
  def boolTuple(r: RNG) : ((Boolean, Boolean), RNG) =
    val (i, r1) = r.nextInt
    val first = i %2 == 0
    val (i1, r2) = r1.nextInt
    val second = i1 % 2 == 0
    ((first, second), r2)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (i, r) = rng.nextInt
    (if i < 0 then -(i + 1) else i, r)

  def double(rng: RNG): (Double, RNG) =
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)


type Rand[+A] = State[RNG, A]

object Rand:
  def int: Rand[Int] = State(_.nextInt)
  def bool: Rand[Boolean] = State(rng => RNG.bool(rng))




/**
 *
 * State is a wrapper on state transition function. State itself is a program or a function
 * This is again a similar pattern where we can decouple program construction or description
 * from execution. Run is the glue between description and execution.
 */
case class State[S, +A](run: S => (A,S)):

  /**
   *
   *Map is simple - given a state transition function, apply f on the output value.
   */
  def map[B](f: A => B): State[S,B] = State(
    s => {
      val (a,s1) = run(s)
      (f(a), s1)
    }
  )

  /**
   *
   * FlatMap is interesting. Get the A from curent STF, using f and A get STF for B
   * Pass the new state to B -> run the STF B.
   * Note that we are still producing STF - which means this is composition!!
   */
  def flatMap[B](f: A => State[S,B]): State[S,B] = State(
    s => {
      val (a,s1) = run(s)
      f(a).run(s1)
    }
  )

  /**
   *
   * Decomposed map2 - we just want to pass the transitioned state from
   * this to sb and finally to sc.
   * Again please remember this is just a composition.
   */
  def simpleMap2[B,C](sb: State[S,B])(f: (A,B) => C): State[S,C] = State(
    s => {
      val (a,s1) = run(s)
      val (b, s2) = sb.run(s1)
      (f(a,b), s2)
    }
  )

  /**
   *This definition is same as above. Lets try to reason it.
   * FlatMap would get A from this SFT -> Line 43.
   * now it passes A to the lambda which creates new STF using sb.
   * Now flatMap runs this new STF passing s1. Which means map or sb uses s2 line 44
   * and produces line 45.
   */
  def map2[B,C](sb: State[S,B])(f: (A,B) => C): State[S,C] =
    this.flatMap(a => sb.map(b => f(a,b)))


object State{

  //defines a State from single value. The state transition function always return same value.
  //State is ignored. But we still have th state which can be used in get/set
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  //we have N state transition function for the domain A. here we compose a STF which
  //would pass state from one STF to another and keep collecting A's.
  //In other way we get a STF which when applied gets all A's and final state!!
  //Again remember this is comoposition!! we are composing a program.
  //how do we do it? map2 already takes care of combining 2 STF
  def sequenceViaFoldRight[S,A](ls: List[State[S,A]]): State[S, List[A]] =
    ls.foldRight(unit[S, List[A]](List.empty))((s,acc) => s.map2(acc)(_ :: _))

  def sequence[S,A](ls: List[State[S,A]]): State[S, List[A]] =
    def loop(ss: List[State[S,A]], s: S, acc: List[A]) : (List[A], S) =
      ss match {
        case Nil => (acc.reverse, s)
        case x :: xs => {
          val (a, s1) = x.run(s)
          loop(xs, s1, a :: acc)
        }
      }
    State(s => loop(ls, s, Nil))

  /**
   *Simple combinator to produce a STF which gives the current state.
   */
  def get[S]: State[S,S] = State(s => (s,s))

  def gets[S,A](f: S => A): State[S,A] = get.map(f)


  /**
   *
   * Set arbitary state -
   */
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify1[S](f: S => S): State[S,Unit] = get.flatMap(s => set(f(s)))

  def modify[S](f: S => S): State[S,Unit] = for{
    s <- get
    x <- set(f(s))
  } yield x

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean,candies: Int,coins: Int)

object Candy:
  import State.{get, modify, sequence}

  def applyInput(i: Input)(s: Machine): Machine =
    (i, s) match {
    case (_, Machine(_, 0, _)) => s
    case (Coin, Machine(false, _, _)) => s
    case (Turn, Machine(true, _, _)) => s
    case (Coin, Machine(true, candy, coin)) =>
    Machine(false, candy, coin + 1)
    case (Turn, Machine(false, candy, coin)) =>
    Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    //lets get all the state modification functions in same order as input.
    val modFuncs: List[Machine => Machine] = inputs.map(applyInput(_))
    //lets get STF in same order - that is STF which would apply modFuncs in same order
    val stfs: List[State[Machine, Unit]] = modFuncs.map(modify(_))
    //lets get the fingle STF which represent applying these STF one after another.

    val finalState: State[Machine, List[Unit]] = sequence(stfs)

    // not just get the values out of state!! state itself remains same.
    finalState.flatMap(_ => get.map(m => (m.candies, m.coins)))


  def simulateMachine1(inputs: List[Input]): State[Machine, (Int, Int)] =
    //lets simplify - applyInput_ is of type - Input => (S=>S)
    //modidy is of type - (S=>S) => State[Machine,Unit]. This is typical function composition
    // g(f(a)) = (f o g)(a)
    val composedFunc: (Input => State[Machine,Unit]) = modify[Machine]  compose applyInput
    for{
      fs <- sequence(inputs.map(composedFunc))
      m <- get
    }yield (m.candies, m.coins)


object Main extends App {
  import State.{get,set}

  def zipIndex[A](ls : List[A]) : List[(Int,A)] =
    val stf = ls.foldLeft(State.unit[Int, List[(Int, A)]](List()))(
      (acc, a) => for{
        xs <- acc
        n <- get
        _ <- set(n + 1)
      }yield (n, a) :: xs
    )
    stf.run(0)._1.reverse

  println(zipIndex((0 to 100000).toList))
}
