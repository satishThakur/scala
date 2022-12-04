package com.satish.examples.foldable
import scala.math.Numeric

object FoldableFuncs:

  def map[A,B](ls: List[A])(f: A => B): List[B] =
    ls.foldRight(List.empty[B])((elem, acc) => f(elem) :: acc)

  def flatMap[A,B](ls : List[A])(f : A => List[B]) : List[B] =
    ls.foldRight(List.empty[B])((elem, acc) => f(elem) ++ acc)

  def filter[A](ls : List[A])(predicate: A => Boolean): List[A] =
    ls.foldRight(List.empty[A])((elem, acc) => if predicate(elem) then elem :: acc else acc)

  def sum[A](ls: List[A])(using numeric: Numeric[A]): A =
    ls.foldRight(numeric.zero)((elem, n) => numeric.plus(elem, n))  