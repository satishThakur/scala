package chap02

object Chap02 {

  def main(args: Array[String]): Unit = {

    1 to 20 foreach(i => println("" + i + ": " + fib(i)))
    //println(isSorted(Array(1,3,50,7))( (a : Int ,b : Int) => a < b) )
  }


  //Given a function which taken N arguments and returns a result - can always be
  //written as a function which takes one less argument --> produces a function which takes that single argument
  // and result

  def curry[A,B,C](f :  (A, B) => C) : A => (B => C) = (a : A) => (b : B) => f(a,b)

  //one more

  def curry[A,B,C,D](f : (A, B, C) => D) : A => B => C => D  = (a : A) => (b : B) => (c : C) => f(a,b,c)

  //this would get the idea we just need to reduce one arg - and then call next one!!
  def curry1[A,B,C,D](f : (A, B, C) => D) : A => B => C => D  = (a : A) => curry((b : B, c: C) => f(a,b,c))

  def uncurry[A,B,C](f : A => B => C) : (A,B) => C = (a : A, b : B) => f(a)(b)

  def uncurry[A,B,C, D](f : A => B => C => D) : (A,B,C) => D = (a : A, b : B, c : C) => f(a)(b)(c)

  def uncurry1[A,B,C, D](f : A => B => C => D) : (A,B,C) => D = (a : A, b : B, c : C) => uncurry(f(a))(b,c)


  //in scala there is a function called compose and - andThen which does this --
  def compose[A,B,C](f :B => C, g: A => B) : A => C = (a : A) => f(g(a))

  //2.1

  def fib(n : Int) : Int = {

    def loop(prev : Int, current : Int, c : Int) : Int = {
      if(c == 0) current else loop(current , prev + current, c - 1)
    }

    if(n == 1) 0 else if(n == 2) 1 else loop(0, 1, n-2)
  }


//2.2
  def isSorted[A](as : Array[A])(ordered : (A,A) => Boolean) = {
    def loop(as : Array[A], index : Int) : Boolean = {
      if(index == as.length - 1) true
      else if(!ordered(as(index), as(index + 1))) false
      else loop(as, index + 1)
    }
    loop(as, 0)
  }

}
