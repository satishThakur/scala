package chap02

sealed trait Tree[+T]

case class Leaf[+T](value : T) extends Tree[T]

case class Node[+T](left : Tree[T], right : Tree[T]) extends Tree[T]


object Tree {

  def size[T](t: Tree[T]): Int = t match {
    case Leaf(_) => 1
    case Node(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Node(l, r) => maximum(l) max maximum(r)
  }


  def depth[T](t: Tree[T]): Int = t match{
    case Leaf(_) => 0
    case Node(l,r) => 1 + (depth(l) max depth(r))
  }

  def map[T,U](t : Tree[T])(f : T => U) : Tree[U] = t match{
    case Leaf(x) => Leaf(f(x))
    case Node(l,r) => Node(map(l)(f), map(r)(f))
  }

  //this function is pretty much like - foldRight - with list
  def fold[T,U](t : Tree[T])(z : T => U)(f : (U, U) => U) : U = t match{
    case Leaf(x) => z(x)
    case Node(l,r) => f(fold(l)(z)(f),fold(r)(z)(f))
  }


  def sizeUsingFold[T](t : Tree[T]) : Int = fold(t)(_ => 1)( 1 + _ + _)

  def maxUsingFold(t : Tree[Int]) : Int = fold(t)(a => a)(_ max _)

  def depthUsingFold[T](t : Tree[T]) : Int =
    fold(t)(_ => 0)( (x,y) => 1 + (x max y))

  def mapUsingFold[T,U](t : Tree[T])(f : T => U) : Tree[U] =
    fold(t)(x => Leaf(f(x)) : Tree[U])(Node(_,_))

}