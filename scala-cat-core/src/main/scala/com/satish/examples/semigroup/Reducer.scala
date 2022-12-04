package com.satish.examples.semigroup




object ReducerMain extends App:
  import cats.Semigroup
  import cats.syntax.semigroup.*
  import cats.instances.int

  def reduce[T](l : List[T])(using sg: Semigroup[T]): T =
    l.reduce(sg.combine)

  case class Expense(id: Long, amount: Long)
  object Expense:
    given esg: Semigroup[Expense] with
      override def combine(x: Expense, y: Expense): Expense =
        Expense(math.max(x.id, y.id), x.amount + y.amount)

  println(reduce(List(1,2,3,4,5)))
  val expense1 = Expense(1,100)
  val expense2 = Expense(2,40)
  println (reduce(List(expense1, expense2)))
