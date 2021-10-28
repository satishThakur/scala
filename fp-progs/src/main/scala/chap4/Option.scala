package chap4

sealed trait Option[+T] {

  //apply f if not None
  def map[U](f: T => U) : Option[U] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }

  //extract the value - and uses default for None
  //unwrap - but with default
  def getOrElse[U >: T](default: => U) : U = this match{
    case Some(x) => x
    case _ => default
  }

  //apply f if not None - and f might fail -
  //Apply map - which we get Option[Option[T]] - Unwrap it -
  def flatMap[U](f: T => Option[U]) : Option[U] = map(f) getOrElse(None)

  /**
    *
    * if we have unit function - Some in this case - map can be derived from flatmap
    * this is intuitive as well map has simple T => U (never fails and is total function)
    * hence a f' can be derived which would be T => Some(U) - :)
    */
  def mapUsingFlatMap[U](f: T => U) : Option[U] = flatMap( t => Some(f(t)))


  /**
    *
    * When wil we use it? Can think of one example - Lets imagine we have a result from cache
    * Now if cache is expired then None or value - So now we can have fallback which itself can actually fail..
    * So to generalize we can chain fallbacks... x orElse(y) orElse(z) getOrElse(Exception)
    * Also observe ob is Non- Strict - and hence never evaluated if original computation was successfully -
    * This would be mathematical definition of fallback :)
    */
  def orElse[U >: T](ob: => Option[U]) : Option[U] = this match{
    case Some(_) => this
    case None => ob
  }


  //self explanatory!!
  def filter(f: T => Boolean) : Option[T] = this match {
    case Some(x) => if(f(x)) None else this
    case None => None
  }

}

case class Some[+T](get : T) extends Option[T]

case object None extends Option[Nothing]



object Option {

  //4.2

  def mean(xs: Seq[Double]): Option[Double] = xs match {
    case Nil => None
    case _ =>Some(xs.sum/xs.length)
  }

  def elemVariances(mean : Double, xs: Seq[Double]): Seq[Double] = {
    xs map(x => math.pow(mean - x, 2))
  }

  def variance(xs: Seq[Double]) : Option[Double] = {

    /**
      * Intution - for the solution  -
      * assume we get mean of list and call it m
      * to get variance all we need is for every element in list map it to squared deviation from mean
      * and just find mean of it - lets call it m'
      * Now m is Option or the result of operation is Option
      * So is m'. Which means both can fail!!
      * because m' produces Option and its input is whatever is wrapped in m - hence we need flatMap from mean(xs)
      */
    mean(xs).flatMap(m => mean(elemVariances(m,xs)))

  }

  /**
    * Intution?
    *
    */
  def variance1(xs: Seq[Double]) : Option[Double] = {
    for{
      m <- mean(xs)
      m1 <- mean(for{
        x <- xs

      }yield math.pow(m-x,2))

    }yield m1
  }

  /**
    *
    * Intuition - We have a function from (a,b) to c, but if we have option[a] and option[b]
    * map2 would convert that to option[c]
    * This also states that if we know how to transform (a,b) => c, we can always transform
    * Option[a], Option[b] to Option without needing any new construct.
    */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = {
    a flatMap (a => b.map(b => f(a,b)))
  }

  //typical sequence impl - as this looks very much fold lets try it with foldRight
  def sequence[A](a : List[Option[A]]) : Option[List[A]] = a match{
    case Nil => Some(Nil)
    case x :: xs => x.flatMap(x => sequence(xs).map(l => x :: l))
  }

  def sequenceFoldRight[A](a : List[Option[A]]) : Option[List[A]] = {
    //here we can see - the function to fold has to options and as want another option - perfect use for map2
    a.foldRight[Option[List[A]]](Some(Nil))( (e, o : Option[List[A]]) => o.flatMap(o1 => e.map(e1 => e1 :: o1)))
  }

  //todo reqFoldUsingMap2
  /**
    *
    * Intuition -
    * we start with Some(EmptyList), now we have 2 optiopns - option of list and option of element -
    * Simple folrRight using map2 with cons function!!
    */
  def sequenceFoldRight1[A](a : List[Option[A]]) : Option[List[A]] = {
    //here we can see - the function to fold has to options and as want another option - perfect use for map2
    a.foldRight[Option[List[A]]](Some(Nil))( (e, o : Option[List[A]]) => map2(e,o)(_ :: _))
  }



  //simple traverse
  /**
    *
    *  Pretty much like sequence we just apply f -
    */
  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match{
    case Nil => Some(Nil)
    case x :: xs => traverse(xs)(f).flatMap(l => f(x).map(x1 => x1::l))
  }

  //traverse using foldright
  def traverseFoldRight[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil))( (e,l) => f(e).flatMap(e1 => l.map(l1 => e1::l1)) )
  }

  //todo use map2
  //traverse using foldright and map
  def traverseFoldRight1[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil))( (e,l) => map2(f(e), l)(_ :: _) )
  }


  //little subtle - intuition?
  /**
    *
    * Well its easy - we want to traverse and do not want to change the inputs so f is identity.
    */
  def seqViaTraverse[A](a : List[Option[A]]) : Option[List[A]] = {
    traverse(a)(identity)
  }


}
