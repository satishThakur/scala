package com.satish.generatos

import java.util.Random



trait Generator[+T]{
  self => //refer to this

  def generate(): T

  def map[U](f: T => U): Generator[U] = new Generator[U] {
    override def generate(): U = f(self.generate)
  }

  def flatMap[U](f: T => Generator[U]): Generator[U] = new Generator[U] {
    override def generate(): U = f(self.generate).generate
  }

}

trait Tree[+T]

case class Inner[+T](left: Tree[T], right: Tree[T]) extends Tree[T]

case class Leaf[+T](data: T) extends Tree[T]

object Generator {

  def single[U](u: U): Generator[U] = new Generator[U] {
    override def generate(): U = u
  }

  def choose(lo: Int, hi: Int) = for{
    x <- integers
  } yield lo + (x % (hi - lo))

  def oneOf[T](xs : T*): Generator[T] = for{
    i <- choose(0,xs.length)
  }yield xs(i)

  val integers: Generator[Int] = new Generator[Int] {
    val rand = new Random
    override def generate() = rand.nextInt

  }

  val booleans: Generator[Boolean] = for{
    x <- integers
  }yield x > 0


  def lists[T](g: Generator[T]): Generator[List[T]] = for{
    b <- booleans
    l <- if(b) emptyLists else nonEmptyList(g)
  } yield l

  def emptyLists[T]: Generator[List[T]] = single(List())

  def nonEmptyList[T](g: Generator[T]): Generator[List[T]] = for{
    head <- g
    tail <- lists(g)
  }yield head :: tail


  def listOfInts: Generator[List[Int]] = lists(integers)


  def treeGen[T](g : Generator[T]): Generator[Tree[T]] = for{
    b <- booleans
    t <- if(b) emptyTree(g) else nonEmptyTree(g)
  }yield t

  def emptyTree[T](g : Generator[T]): Generator[Tree[T]] = for{
    t <- g
  }yield Leaf(t)

  def nonEmptyTree[T](g: Generator[T]): Generator[Tree[T]] = for{
    l <- treeGen(g)
    r <- treeGen(g)
  }yield Inner(l,r)



}



import Generator._
object App{

  def main(args: Array[String]): Unit = {
    for(i <- 1 to 10){
      val l = listOfInts.generate
     // println(l)
    }
    for(i <- 1 to 10) {
      val t = treeGen(integers).generate

      println(t)
    }


  }


}
