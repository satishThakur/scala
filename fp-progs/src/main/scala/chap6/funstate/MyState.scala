package chap6.funstate

/**
  *
  * State is modelled as a function which given the present state produces the value (could be any computation)
  * and return the new state.
  * Take example of any stateful system - like database.
  * So here state is current state of db lets say and it produces any query result - now that could mutate db
  * to another state which is returned back.
  * Kind of functional db!!
  *
  */
trait MyState[S, +A] extends (S => (A,S)){

  /**
    *
    * Now in relation to State map produces another State given we have function from A to B
    * Simple map
    */
  def map[B](f: A => B): MyState[S,B] = s => {
    val (v,s1) = apply(s)
    (f(v), s1)
  }

  /**
    *
    * Same as map but here we have a function which can produce another State from value a.
    */
  def flatMap[B](f: A => MyState[S,B]): MyState[S,B] = s => {
    val (v,s1) = apply(s)
    f(v)(s1)
  }

}

object MyState {

  /**
    *
    * As the name suggests creates State from single value - State that always return the same value.
    */
  def unit[S,A](a: A): MyState[S,A] = s => (a, s)


  /**
    *
    * Given we have multiple state actions - sequence will run them one after another - feeding
    * state of last to next and in process would accumulate the results as well.
    */

  def sequence[S,A](ssa: List[MyState[S,A]]): MyState[S, List[A]] = s => {
    def loop(s: S, ssa: List[MyState[S,A]], acc: List[A]): (List[A],S) = ssa match {
      case Nil => (acc.reverse,s)
      case x::xs => {
        val (v,s1) = x(s)
        loop(s1, xs, v::acc)
      }
    }
    loop(s,ssa, List())
  }

  /**
    *
    * Same sequence but idiomatic way via foldRight -
    */
  def sequenceViaFoldRight[S,A](ssa: List[MyState[S,A]]): MyState[S, List[A]] = s => {
    ssa.foldRight[(List[A],S)]((List(),s))((ss, l) => {
      val (ll ,pr) = l
      val (v, s1) = ss(pr)
      (v :: ll,s1)
    })
  }


  /**
    *
    * get is simple combinator which given a state S gives a Mystate which would produce as S when applied
    * Why do we need get ? get produces MyState of state S itself - which would let us compose on state rather than
    * what we produces. for example get.map -> will act on state. Same for flatmap.
    * So the combinators defined on state class let us compose in term of output .
    */
  def get[S]: MyState[S,S] = s => (s,s)

  /**
    *
    * Again this a combinator which would set the state to the given value
    * Why we need set? same as get - if there is certain operation which can
    * arbitarily change state lets say some function f - then set let us compose to produce
    * the desired state.
    * simple example would be get.map -> [  get.map(s => set(f(s))) ] -> flatmap would make more sense
    * --see modify
    */

  def set[S](s: S): MyState[S, Unit] = _ => ((), s)


  /**
    *
    * modify is the combinator which would get the current state and then apply the function on that and
    * set it as new state -
    */
  def modify[S](f: S => S): MyState[S, Unit] = get.flatMap(s => set(f(s)))

  /**
    *
    * Same as above but this explains how get and set combinator for state transition are basic building blocks
    */
  def modifyUsingFor[S](f: S => S): MyState[S, Unit] = for{
    s <- get
   s1 <- set(f(s))
  }yield s1


}

import MyState._

/**
  * Machine has 2 inputs - either insert a coin or turn a knob to dispense the candy.
  * Machine will only dispense candy in unlocked state.
  * To unlock machine from locked state coin insertion is needed.
  */

sealed trait Input
case object Coin extends Input
case object Turn extends Input

/**
  *
  * @param locked - machine is locked/unlocked
  * @param candies - number of candies in current system
  * @param coins - number of coins in the machine
  *
  * Rules:
  * Insert a coin to locked machine will make it unlocked
  * Turning a knob on unlocked will give candy and lock the machine
  * Turning a unlocked machine OR inserting coin on unlocked machine does nothing (may be the coin falls to outside
  * but lets not bother!!)
  */
case class CMachine(locked: Boolean, candies: Int, coins: Int)


object CandyMachine{


  def applyInput = (i: Input) => (m: CMachine) => (i,m) match {
    case (_, CMachine(_,0,_)) => m //if no candies machine remain in same state - locked!!
    case (Turn, CMachine(true,_,_)) => m
    case (Coin, CMachine(false,_,_)) => m // we are honest we did not increase number of coins!!
    case (Coin, CMachine(true, candy, coin)) => CMachine(false,candy,coin + 1)
    case (Turn,CMachine(false,candy,coin)) => CMachine(true, candy - 1, coin)

  }

  def simulateMachine(inputs: List[Input]): MyState[CMachine, (Int, Int)] = for {
    _ <- sequence(inputs.map( modify[CMachine]_ compose applyInput ))
    s1 <- get
  } yield (s1.candies, s1.coins)


  def simulateMachineOneAction(input: Input): MyState[CMachine, (Int, Int)] = {
    val stateFunc: (CMachine => CMachine) = applyInput(input)
    val currentState: MyState[CMachine,CMachine] = get
    val setState = currentState.flatMap(s => set(stateFunc(s)))
    setState.flatMap(_ => get[CMachine].map(m1 => (m1.candies,m1.coins)))
  }

  def simulateNMachineActions(inputs: List[Input]): MyState[CMachine, (Int, Int)] = {
    val states: List[MyState[CMachine, (Int, Int)]]  = inputs map simulateMachineOneAction

    val st: MyState[CMachine, List[(Int,Int)]] = sequence(states)

    st.flatMap(_ => get.map(m1 => (m1.candies,m1.coins)))
  }
}




object App{
  def main(args: Array[String]): Unit = {

    val (s,m) = CandyMachine.simulateMachine(List(Coin,Turn,Coin,Turn,Turn,Coin,Turn))(CMachine(true, 10, 3))
    println(s)

    val (s1,m1) = CandyMachine.simulateMachineOneAction(Coin)(CMachine(true, 10, 3))
    println(s1)



    val (s2,m2) = CandyMachine.simulateNMachineActions(List(Coin,Turn,Coin,Turn,Turn,Coin,Turn))(CMachine(true, 10, 3))
    println(s2)





    /*
    println("Hello there")

    val s: MyState[Int, Int] = i => (i, i +1)

    println(s(4)._1)
    //todo what if I want nth value and the state - not intermediate.

    val sqComb = MyState.modifyUsingFor[Int](i => i * i)

    println(s(5)._2)
    println(sqComb(s(5)._2)._2)


    /**
      * These examples demonstate how map, flatmap can be used to produce
      * State for other types from one type.
      */
    def example(s : MyState[Int,String]): MyState[Int,Char] = for{
      st <- s
    } yield st.last

    def example1(s : MyState[Int,String]): MyState[Int,Char] = for{
      st <- s
      s1 <- MyState.unit(st.last)
    } yield s1

     */

  }
}
