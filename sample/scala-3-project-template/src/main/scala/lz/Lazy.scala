package lz

import lz.MyStream.{cons, fromFib}

import scala.annotation.tailrec

/**
 * We want to implement a Lazy stream which can potentially be infinite. What are the abstractions
 * which we can use?
 * Thunks -> expression which is evaluated lazily.
 * By Name -> convinient Thunk in Scala
 * Lazy val -> this is more of optimization
 * Key principle - in any of the function tail should not get evaluated --> Mental model. We always evaluate
 * head and tail remains lazy. Infact tail is a program!! A descriptipn. 
 *
 * @tparam A
 */
trait MyStream[+A]:
  def empty: Boolean = this match {
    case Cons(_, _) => false
    case _ => true
  }
  def headOption: Option[A] = this match {
    case Cons(h, _) => Some(h())
    case _ => None
  }

  def tailOption: Option[MyStream[A]] = this match {
    case Cons(_, t) => Some(t())
    case _ => None
  }

  def #::[B >: A](element: =>B): MyStream[B] = cons(element, this)
  def ++[B >: A](another: => MyStream[B]): MyStream[B] = this match {
    case Empty => another
    case Cons(h, t) => cons(h(), t() ++ another)
  }
  def map[B](f: A => B): MyStream[B] = this match{
    case Empty => Empty
    case Cons(h, t) => cons(f(h()), t() map f)
  }
  def flatMap[B](f: A => MyStream[B]): MyStream[B] = this match{
    case Empty => Empty
    case Cons(h, t) => f(h()) ++ t().flatMap(f)
  }
  def filter(f: A => Boolean): MyStream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if f(h()) then cons(h(), t().filter(f)) else t().filter(f)
  }

  def takeN(n: Int): List[A] = {
    if n == 0 then Nil else
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().takeN(n -1)
    }
  }

case class Cons[A](h: () => A, t: () => MyStream[A]) extends MyStream[A]
case object Empty extends MyStream[Nothing]

//object Empty extends MyStream[Nothing]:


object MyStream{
  //both has to be lazy so that we delay any computation.
  def cons[A](head: => A, tail: => MyStream[A]): MyStream[A] = {
    //this is just optimization so that we do not do a heavy computation again.
    lazy val h = head
    lazy val t = tail
    Cons(() => h, () => t)
  }

  def unfold[A,S](z: S)(f: S => Option[(A,S)]): MyStream[A] =
    f(z) match {
      case Some((a,s)) => cons(a, unfold(s)(f))
      case None => Empty
    }

  def from[A](start: =>A, gen: A => A): MyStream[A] = {
    unfold(start)(x => Some((x, gen(x))))
  }

  def fromFib(f: Int, s: Int): MyStream[Int] = {
    unfold((f,s)){
      case (e1, e2) => Some((e1, (e2, e1 + e2)))
    }
    //cons(f, fromFib(s, f + s))
  }


  def primes: MyStream[Int] =
    def primes0(start: MyStream[Int]): MyStream[Int] =
      val hd = start.headOption.get
      val tl = start.tailOption.get
      cons(hd, primes0(tl.filter(_ % hd != 0)))
    primes0(from(2, _ + 1))

}


object Lz extends App {
  println("hello world")

  val numbers = MyStream.from(0, _ + 1)
  numbers.takeN(5).foreach(println)

  val another = numbers ++ numbers
  val nn = -20 #:: -25 #:: another
  println("**nn**")
  nn.takeN(5).foreach(println)

  //Fn = Fn-1 + Fn-2
  def fabbinacciStream: MyStream[Int] = fromFib(0,1)

  println("**Fib**")
  fabbinacciStream.takeN(100).foreach(println)

  println("**Primes**")
  MyStream.primes.takeN(15).foreach(println)


}
