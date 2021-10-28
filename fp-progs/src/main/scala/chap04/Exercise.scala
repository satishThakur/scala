package chap04

object Exercise {


  def mean(ls : Seq[Double]) : Option[Double] = ls match {
    case Nil => None
    case _ => Some(ls.sum/ls.length)
  }

  //4.2
  def variance(ls : Seq[Double]) : Option[Double]= {

    mean(ls) flatMap (m => mean(ls.map(l => math.pow(l - m, 2))))
  }



  def lift[A,B](f: A=>B) : Option[A] => Option[B] = _ map f


  def Try[A](exp: => A) : Option[A] = {
    try Some(exp)
    catch {case _ => None}
  }

  //4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C) : Option[C] = (a,b) match {
    case (_, None) => None
    case (None, _) => None
    case (Some(x), Some(y)) => Some(f(x,y))
  }

  //intution - a is already Option and result of this function is also option - hence we need flatmap
  def map2UsingFlatMap[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C) : Option[C] = {

    a.flatMap(a1 => b.map(b1 => f(a1, b1)))

  }

  //4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match{
    case Nil => Some(Nil)
    case x :: xs => x flatMap( x1 => sequence(xs).map(l => x1 :: l))
  }

  //4.5 - bad implementation - run time efficiency is poor -
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
     sequence(a.map(x => f(x)))
  }

  def traverse1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match{
    case Nil => Some(Nil)
    case x :: xs => f(x) flatMap(x1 => traverse1(xs)(f).map(l => x1 :: l))
  }

  //TODO implement both traverse and sequence using foldRight - does it fit intution?





}
