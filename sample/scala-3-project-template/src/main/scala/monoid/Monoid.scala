package monoid

import testing.Exercise.{Gen, Prop}
import parallel.Par.{Par, lazyUnit, map2, parMap, unit}

trait Monoid[A]:
  def zero: A
  def op(a1: A, a2: A): A

object Monoid:
  val intAddition: Monoid[Int] = new Monoid[Int]:
    override def zero: Int = 0
    override def op(a1: Int, a2: Int): Int = a1 + a2

  val intMult: Monoid[Int] = new Monoid[Int]:
    override def zero: Int = 1
    override def op(a1: Int, a2: Int): Int = a1 * a2

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean]:
    override def zero: Boolean = true
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean]:
    override def zero: Boolean = false
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

  def optionMonoid[A](option: Option[A]): Monoid[Option[A]] = new Monoid[Option[A]]:
    override def zero: Option[A] = None
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

  def dual[A](m : Monoid[A]): Monoid[A] = new Monoid[A] {
    def zero = m.zero
    def op(a: A, b: A): A = m.op(b, a)
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A]:
    override def zero: A => A = identity
    override def op(a1: A => A, a2: A => A): A => A = a => a2(a1(a))

  //10.5
  def foldMap[A,B](a: List[A], m: Monoid[B])(f: A => B): B =
    a.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  //10.6 - TODO revisit.
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    val m: Monoid[B => B] = endoMonoid[B]
    foldMap(as, m)(f.curried)(z)

  //10.4
  def moniodLaws[A](m: Monoid[A], g: Gen[A]): Prop = {
    val triGen = for{
      a <- g
      b <- g
      c <- g
    }yield (a,b, c)
    Prop.forAll(triGen){
      (a,b,c) => m.op(m.op(a,b), c) == m.op(a, m.op(b,c))
    } &&
    Prop.forAll(g){
      a => m.op(m.zero, a) == a && m.op(a, m.zero) == a
    }
  }

  //10.7 -
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if v.size == 0 then m.zero else if v.size == 1 then f(v(0))
    else
      val (l,r) = v.splitAt(v.size / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]]:
    override def zero: Par[A] = unit(m.zero)
    override  def op(a: Par[A], b: Par[A]): Par[A] = map2(a, b)(m.op)


  def parFoldM[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    //val pm = parMap(v.toList)(f)
    foldMapV(v map f, par(m))(a => lazyUnit(a))
  }

  def productMonoid[A,B](a: Monoid[A], b: Monoid[B]): Monoid[(A,B)] = new Monoid[(A,B)] {
    def zero: (A,B) = (a.zero, b.zero)
    def op(o1: (A,B), o2: (A,B)): (A,B) =
      (a.op(o1(0), o2(0)), b.op(o1(1), o2(1)))
  }

  def functionMonoid[A,B](m : Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def zero: A => B = _ => m.zero
    def op(f1: A => B, f2: A => B): A => B =
      a => m.op(f1(a), f2(a))
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC]:
    def zero = Stub("")
    def op(a: WC, b: WC): WC = (a, b) match {
      case (Stub(pa), Stub(pb)) => Stub(pa + pb)
      case (Stub(pa), Part(ls, wc, rstub)) => Part(pa + ls, wc, rstub)
      case (Part(ls, wc, rstub),Stub(pa) ) => Part(ls, wc, rstub + pa)
      case (Part(ls, wc, rstub),Part(ls1, wc1, rstub1) ) =>
        Part(ls, wc + (if (rstub + ls1).isEmpty then 0 else 1) + wc1 , rstub1)
    }
  def count(s: String): Int =
    def wc(c: Char): WC =
      if c.isWhitespace then Part("", 0, "") else Stub(c.toString)

    def unstub(s : String): Int = if s.isEmpty then 0 else 1
    val foldedWC: WC = foldMapV(s.toIndexedSeq, wcMonoid)(wc)
    foldedWC match {
      case Stub(s) => unstub(s)
      case Part(l, wc, r) => unstub(l) + wc + unstub(r)
    }


trait Foldable[F[_]]:
  import Monoid.*
  //foldRight
  extension[A](fa: F[A])
    def foldRight[B](zero: B)(f: (A,B) => B): B =
      fa.foldMap(f.curried)(using endoMonoid[B])(zero)
  //foldLeft
    def foldLeft[B](zero: B)(f: (B,A) => B): B =
      fa.foldMap(a => b => f(b,a))(using dual(endoMonoid[B]))(zero)

    def foldMap[B](f: A => B)(using m: Monoid[B]): B =
      fa.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

    def toList: List[A] =
      fa.foldRight[List[A]](Nil)(_ :: _)

given ListFoldable: Foldable[List] with
  extension[A](fa: List[A])
    override def foldRight[B](zero: B)(f: (A, B) => B): B =
      fa.foldRight(zero)(f)
    override def foldLeft[B](zero: B)(f: (B,A) => B): B =
      fa.foldLeft(zero)(f)
    override def toList: List[A] = fa

given IndexedSeqFoldable: Foldable[IndexedSeq] with
  import Monoid.foldMapV

  extension[A](fa: IndexedSeq[A])
    override def foldRight[B](zero: B)(f: (A, B) => B): B =
      fa.foldRight(zero)(f)

    override def foldLeft[B](zero: B)(f: (B,A) => B): B =
      fa.foldLeft(zero)(f)

    override def foldMap[B](f: A => B)(using m: Monoid[B]): B =
      foldMapV(fa, m)(f)

    override def toList: List[A] = fa.toList

given LazyListFoldable: Foldable[LazyList] with
  extension[A](fa: LazyList[A])
    override def foldRight[B](zero: B)(f: (A, B) => B): B =
      fa.foldRight(zero)(f)
    override def foldLeft[B](zero: B)(f: (B,A) => B): B =
      fa.foldLeft(zero)(f)


sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

given TreeFoldable: Foldable[Tree] with
  extension[A](fa: Tree[A])
    override def foldLeft[B](zero: B)(f: (B,A) => B): B = fa match {
      case Leaf(v) => f(zero, v)
      case Branch(l, r) =>
        val rightReduced = r.foldLeft(zero)(f)
        l.foldLeft(rightReduced)(f)
    }

  //does not use m.zero...
    override def foldMap[B](f: A => B)(using m: Monoid[B]): B = fa match {
      case Leaf(v) => f(v)
      case Branch(l, r) => m.op(l.foldMap(f), r.foldMap(f))
    }

    override def foldRight[B](zero: B)(f: (A,B) => B): B = fa match {
      case Leaf(v) => f(v, zero)
      case Branch(l , r) =>
        val rightReduced = r.foldRight(zero)(f)
        l.foldRight(rightReduced)(f)
    }


given OptionFoldable: Foldable[Option] with
  extension[A](fa: Option[A])
    override def foldRight[B](zero: B)(f: (A,B) => B): B = fa match {
      case None => zero
      case Some(a) => f(a,zero)
    }

  //foldLeft
    override def foldLeft[B](zero: B)(f: (B,A) => B): B = fa match {
      case None => zero
      case Some(a) => f(zero, a)
    }

    override def foldMap[B](f: A => B)(using m: Monoid[B]): B = fa match {
      case None => m.zero
      case Some(a) => f(a)
  }


object Main extends App:
  def stringMonoid = new Monoid[String]:
    override def zero: String = ""
    override def op(a1: String, a2: String): String = a1 + a2

