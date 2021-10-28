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

  def foldMap[A,B](a: List[A], m: Monoid[B])(f: A => B): B =
    a.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

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
      (a.op(o1(0), o1(0)), b.op(o1(1), o2(1)))
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
  def foldRight[A,B](fa: F[A])(zero: B)(f: (A,B) => B): B =
    foldMap(fa)(endoMonoid[B])(f.curried)(zero)

  //foldLeft
  def foldLeft[A,B](fa: F[A])(zero: B)(f: (B,A) => B): B =
    foldMap(fa)(dual(endoMonoid[B]))(a => b => f(b,a))(zero)

  def foldMap[A,B](fa: F[A])(m: Monoid[B])(f: A => B): B =
    foldLeft(fa)(m.zero)((b, a) => m.op(b, f(a)))

  def toList[A](as: F[A]): List[A] =
    foldRight[A,List[A]](as)(Nil)(_ :: _)

object ListFoldable extends Foldable[List]:
  override def foldRight[A, B](fa: List[A])(zero: B)(f: (A, B) => B): B =
    fa.foldRight(zero)(f)
  override def foldLeft[A,B](fa: List[A])(zero: B)(f: (B,A) => B): B =
    fa.foldLeft(zero)(f)
  override def toList[A](as: List[A]): List[A] = as

object IndexedSeqFoldable extends Foldable[IndexedSeq]:
  import Monoid.foldMapV

  override def foldRight[A, B](fa: IndexedSeq[A])(zero: B)(f: (A, B) => B): B =
    fa.foldRight(zero)(f)

  override def foldLeft[A,B](fa: IndexedSeq[A])(zero: B)(f: (B,A) => B): B =
    fa.foldLeft(zero)(f)

  override def foldMap[A,B](fa: IndexedSeq[A])(m: Monoid[B])(f: A => B): B =
    foldMapV(fa, m)(f)

  override def toList[A](as: IndexedSeq[A]): List[A] = as.toList

object LazyListFoldable extends Foldable[LazyList]:
  override def foldRight[A, B](fa: LazyList[A])(zero: B)(f: (A, B) => B): B =
    fa.foldRight(zero)(f)
  override def foldLeft[A,B](fa: LazyList[A])(zero: B)(f: (B,A) => B): B =
    fa.foldLeft(zero)(f)


sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree]:
  override def foldLeft[A,B](fa: Tree[A])(zero: B)(f: (B,A) => B): B = fa match {
    case Leaf(v) => f(zero, v)
    case Branch(l, r) =>
      val rightReduced = foldLeft(r)(zero)(f)
      foldLeft(l)(rightReduced)(f)
  }

  //does not use m.zero...
  override def foldMap[A,B](fa: Tree[A])(m: Monoid[B])(f: A => B): B = fa match {
    case Leaf(v) => f(v)
    case Branch(l, r) => m.op(foldMap(l)(m)(f), foldMap(r)(m)(f))
  }

  override def foldRight[A,B](fa: Tree[A])(zero: B)(f: (A,B) => B): B = fa match {
    case Leaf(v) => f(v, zero)
    case Branch(l , r) =>
      val rightReduced = foldRight(r)(zero)(f)
      foldRight(l)(rightReduced)(f)
  }


object OptionFoldable extends Foldable[Option]:
  override def foldRight[A,B](fa: Option[A])(zero: B)(f: (A,B) => B): B = fa match {
    case None => zero
    case Some(a) => f(a,zero)
  }

  //foldLeft
  override def foldLeft[A,B](fa: Option[A])(zero: B)(f: (B,A) => B): B = fa match {
    case None => zero
    case Some(a) => f(zero, a)
  }

  override def foldMap[A,B](fa: Option[A])(m: Monoid[B])(f: A => B): B = fa match {
    case None => m.zero
    case Some(a) => f(a)
  }


object Main extends App:
  def stringMonoid = new Monoid[String]:
    override def zero: String = ""
    override def op(a1: String, a2: String): String = a1 + a2

