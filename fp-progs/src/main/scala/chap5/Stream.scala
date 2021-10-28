package chap5

import Stream._


object App{
  def main(args: Array[String]): Unit = {

    val lz = Stream(1,2,3,4,5, 10 ,15, 9, 6, 8)
    println(lz.toList)

    println(lz.takeWhile(_ < 10).toList)

    println(lz.takeWhileUsingFoldRight(_ < 10).toList)

    println(lz.headOption)

    println(lz.headOptionUsingFoldRight)

    val ones = constant(1)
    println(ones.exists( _ % 2 != 0))

    println(from(10).take(4).toList)

    println(fibs.take(10).toList)

    println(fibsUsingUnfold.take(10).toList)

    println(onesUsingUnfold().take(10).toList)

    println(lz.mapUsingUnfold(_ + 2).toList)

    println(lz.takeUsingUnflod(4).toList)


    println(lz.tails.take(3))

  }
}


sealed trait Stream[+T]{

  def headOption: Option[T] = this match{
    case Empty => None
    case Cons(x,_) => Some(x())
  }

  def toList: List[T] = this match{
    case Empty => Nil
    case Cons(x,xs) => x() :: xs().toList
  }

  def find(f: T => Boolean): Option[T] = this match{
    case Empty => None
    case Cons(x,xs) => if (f(x())) Some(x()) else xs().find(f)
  }

  def take(n: Int): Stream[T] = this match{
    case Cons(x, xs) if n > 0 => cons(x(), xs().take(n-1))
    case _ => Empty
  }

  def drop(n : Int): Stream[T] = this match{
    case Cons(_, xs) if n > 0 => xs().drop(n -1)
    case _ => this
  }

  def takeWhile(p : T => Boolean): Stream[T] = this match{
    case Cons(x,xs) if p(x()) => cons(x(), xs().takeWhile(p))
    case _ => Empty
  }

  /**
    *
    * Point to be noted - for f second arg is non - strict - hence if f can make decision solely based on
    * first arg - it can terminate early - which is not possible in strict version of foldRight
    * See how exists uses this.
    */
  def foldRight[U](z: =>U)(f: (T, =>U) => U): U = this match{
    case Cons(x,xs) => f(x(), xs().foldRight(z)(f))
    case _ => z
  }

  def exists(p: T => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b )


  def forAll(p: T => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b )

  /**
    *
    * Looks wrong at the first go but - keep in mind it is stream and hence in f we always make
    * decision on t - and hence s is never evaluated!!
    * So if p(t) we return cons (t,s) ==> s is unevaluated!!
    */
  def takeWhileUsingFoldRight(p : T => Boolean): Stream[T] = {
    foldRight(empty[T])( (t,s) => if(p(t)) cons(t,s) else empty)
  }


  def headOptionUsingFoldRight: Option[T] = {
    foldRight(None: Option[T])((t,_) => Some(t))
  }

  def map[U](f: T => U): Stream[U] = foldRight(empty[U])( (t, s) => cons(f(t),s))

  def filter(f: T => Boolean): Stream[T] = foldRight(empty[T])( (t, s) => if(f(t)) cons(t, s) else s)

  def append[B>:T](s: => Stream[B]): Stream[B] = foldRight(s)( (t,s) => cons(t,s))

  def flatMap[U](f: T => Stream[U]): Stream[U] = foldRight(empty[U])( (t,s) => f(t).append(s))


  def mapUsingUnfold[U](f: T => U): Stream[U] = {
    unfold(this){
      case Cons(x, xs) => Some(f(x()), xs())
      case Empty => None
    }
  }

  def takeUsingUnflod(n: Int): Stream[T] = {
    unfold( (this,n) ){
      case (Cons(x,xs),n1) if n1 > 0 => Some(x(), (xs(), n1-1) )
      case _ => None
    }
  }

  def takeWhileUsingUnfold(p : T => Boolean): Stream[T] = {

    unfold(this){
      case Cons(x, xs) if p(x()) => Some(x(), xs())
      case _ => None
    }
  }

  def zip[B](s2: Stream[B]): Stream[(T,B)] = zipWith(s2)( (_,_))

  def zipWith[B,C](s2: Stream[B])(f: (T,B) => C): Stream[C] = {
    unfold( (this, s2) ){
      case ( Cons(g,h), Cons(t,u)) => Some(f(g(), t()), (h(), u()))
      case _ => None
    }
  }

  //this can be generalized -
  //todo
  def zipAll[U](s2: Stream[U]): Stream[(Option[T], Option[U])] = {

    unfold((this, s2)) {
      case (Cons(g, h), Cons(t, u)) => Some((Some(g()), Some(t())), (h(), u()))
      case (Empty, Cons(t, u)) => Some((None, Some(t())), (empty, u()))
      case (Cons(g, h), Empty) => Some((Some(g()), None), (h(), empty))
      case (Empty, Empty) => None

    }
  }


  def startsWith[A](s: Stream[A]): Boolean = {
      zipAll(s).takeWhile(!_._2.isEmpty) forAll {
        case (h, h2) => h == h2
      }
  }

  def tails: Stream[Stream[T]] = {
      unfold(this){
        case Cons(g,h) => Some( (this, h()))
        case Empty => None
      } append Stream(empty)

  }

  def scanRight[U](z: U)(f: (T, => U) => U): Stream[U] = {
    foldRight((z, Stream(z)))( (a,p0) => {
      lazy val p1 = p0
      val v = f(a, p1._1)
      (v, cons(v, p1._2))
    })._2
  }

}

case class Cons[+T](hd : () => T, t : () =>Stream[T]) extends Stream[T]

case object Empty extends Stream[Nothing]

object Stream {

  def empty[T]: Stream[T] = Empty

  //apply has normal values and not by name etc -
  //why is as nor (=>T)* ?
  def apply[T](as : T*): Stream[T] = {

    if(as.isEmpty) empty else cons(as.head, apply(as.tail : _*))
  }

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def fibs0(curr: Int, next: Int): Stream[Int] = cons(curr, fibs0(next, curr + next))
    fibs0(0,1)
  }

  //magic starts here - we create cons with a thunk but which are closure over head and tail
  // which only gets evaluated only once. Had it been trunk with h and t - they would have been
  // computed everytime they are needed.
  def cons[T](h : =>T, t : =>Stream[T]): Stream[T] = {
    lazy val head = h
    lazy val tail = t
    new Cons(() =>head, () => tail)
  }

  /**
    *
    * So given initial state and a function - which produces element of stream as well as next state.
    *
    *
    */
  def unfold[A,S] (z: S) (f: S => Option[(A,S)]): Stream[A] = f(z) match{
    case Some((h,t)) => cons(h, unfold(t)(f))
    case None => empty[A]
  }





  def fibsUsingUnfold(): Stream[Int] = unfold[Int,(Int,Int)]((0,1)){case (f0, f1) => Some((f0, (f1, f0 + f1)))}

  def onesUsingUnfold(): Stream[Int] = unfold[Int,Int](1)(_ => Some(1,1))

  def constantUsingUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a,a))

}
