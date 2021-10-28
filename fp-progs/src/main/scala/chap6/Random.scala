package chap6



object Random {

  //6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, r) = rng.nextInt
    (if (n < 0) -(n + 1) else n , r)
  }

  //6.2

  def double(rng: RNG): (Double, RNG) = {
    val (n,r) = nonNegativeInt(rng)

    (n / (Int.MaxValue.toDouble + 1), r)
  }

  //6.3

  def intDouble(rng: RNG): ( (Int,Double), RNG) = {
    val (i, r) = rng.nextInt

    val (d,r1) = double(r)
    ((i,d), r1)
  }

  def doubleInt(rng: RNG): ( (Double,Int), RNG) = {
    val ((i,d), r) = intDouble(rng)
    ((d,i), r)
  }

  def double3(rng: RNG): ((Double, Double,Double), RNG) = {
    val (d1,r1) = double(rng)
    val (d2,r2) = double(r1)
    val (d3,r3) = double(r2)
    ((d1,d2,d3),r3)
  }

  //6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if(count == 0) (Nil, rng)
    else{
      //lets use the given rng to calculate first value of list -
      val (i,r) = rng.nextInt
      //pass the generated r recursively.
      val (l,r1) = ints(count - 1)(r)
      (i :: l,r1)
    }
  }

 // type State[S,+A] = S => (A,S)

  //represent Random Generator of any type - keeping input as constant - it only uses RNG!!
  type Rand[+A] = RNG => (A, RNG)

  //VERY base type which is by default implemented
  val int: Rand[Int] = _.nextInt

  //construct from single value. Here rng is not really used!!
  //this is fundamental way of constructing Rand from unit value - it is like constructor..
  def unit[A](a: A): Rand[A] = rng => (a,rng)

  //simple map -
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, r) = s(rng)
      (f(a), r)
    }
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a,r) = f(rng)
      g(a)(r)
    }
  }


  /**
    * intuition?
    * we already have function form A to B, now flatmap needs A to Rand[B]
    * now we have A and we have f when we apply we already have B
    * What does Rand[B] does it just spits out B - so here we can use unit to wrap f(a)
    *
    */
  def mapUsingFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))


  //typical flatmap + map combination.
  def map2UsingFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a,b)))
  }

  //better 6.5
  def doubles : Rand[Double] = map(int)(_ / (Int.MaxValue.toDouble + 1))

  //6.5
  def doubleUsingMap(rng: RNG): (Double, RNG) = {
    map(int)(_ / (Int.MaxValue.toDouble + 1))(rng)
  }

//6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = {
    rng => {
      val (a,r) = ra(rng)
      val(b,r1) = rb(r)
      (f(a,b),r1)
    }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)( (_,_))

  //6.7


  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match{
    case Nil => rnd => (Nil, rnd)
    case x::xs => map2(x, sequence(xs))(_ :: _)
  }

  def sequenceUsingFoldRight[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((e,l) => map2(e,l)(_ :: _))
  }

  def _ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))







  def main(args: Array[String]): Unit = {
    println(4)
    val x = SimpleRNG(42)

    val (n1,r1) = x.nextInt
    println(n1,r1)

    val (n2,r2) = r1.nextInt
    println(n2,r2)

    println(Int.MinValue, Int.MaxValue)

    println(ints(10)(SimpleRNG(42)))

    println(_ints(10)(SimpleRNG(42)))

    val s: State[Int, Int] = State(s => (s, s +1))

    //s.run(5)._1

    //val set = State.set[Int](100)


    println(Candy.simulateMachine(List(Coin, Turn, Coin,Turn,Coin,Turn,Turn)).run(Machine(true, 5,10))._1)

  }
}


/**
  * return state along with the random number -
  * State without side effect - rather than mutating the state - we return the new state back.
  */
trait RNG{
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
    val nextRNG = SimpleRNG(newSeed) // The next state, which is an `RNG` instance created from the new seed.
    val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
    (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
  }
}

import State._

/**
  *
  * Intuition - State represent S state which can produce A. Now when it produces A State mutates
  * But State class is functional State hence it provides you the new State as output
  */
case class State[S,+A](run: S => (A,S)){

  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def map2[B,C](sb: State[S,B])(f: (A,B) => C): State[S,C] = {
    flatMap(a => sb.map(b => f(a,b)))
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(
    s => {
      val (a,s1) = this.run(s)
      f(a).run(s1)
    }
  )

}

object State{

  def unit[S,A](a: A): State[S,A] = State(s => (a,s))

  //what does sequence do? it applies one state after another passing on the new state down
  //and we get back final list as well as all the values generated so far!!
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {

    def go(s: S, sas: List[State[S, A]], acc: List[A]): (List[A],S) = {
      if (sas.isEmpty) (acc.reverse,s)
      else{
        val (a,s1) = sas.head.run(s)
        go(s1, sas.tail, a :: acc)
      }
    }
    State(s => go(s, sas, List()))
  }

  def get[S]: State[S, S] = State(s => (s,s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    p <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield p


  def modify1[S](f: S => S): State[S, Unit] = get.flatMap(s => set(f(s)))

  //modify can be implemented directly but get and set let you compose in for expression etc.
  //we might create another abstraction like modify easily using get and set combinators.
  def modify2[S](f: S => S): State[S, Unit] = State(
    s => {
      ((),f(s))
    }
  )

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    //look at simulateMachine1 down here modify[Machine]_  is just syntax to convert method to function - scala thing
    _ <- sequence(inputs map (modify[Machine]_  compose update))
    s <- get
  } yield (s.coins, s.candies)


  def simulateMachine1(inputs: List[Input]): State[Machine, (Int, Int)] = {
    //from list of inputs lets get list of state mapping functions
    val x: List[Machine => Machine] = inputs.map(update)

    //now lets get list of all states by updating state using the mappings.
    //now we have list of all states we would have when we would have applied the inputs.
    val y: List[State[Machine, Unit]] = x.map(f => modify(f))

    //now we need the final state - so lets apply all states using sequence.
    val z: State[Machine,List[Unit]] = sequence(y)

    //now only thing we need is extract the state - which is its coins and candies.
    //we only care about state - we need a function from Machine to State[Machine, (int,int)]
    //so we take the same state -> which is what get does and map the state to its coin and candy.
    z.flatMap(_ => get.map(s => (s.coins,s.candies)) )
  }

}




