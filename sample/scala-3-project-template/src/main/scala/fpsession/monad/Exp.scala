package fpsession.monad

/**
Build an idea from evaluating an simple expression to Monad
**/

trait Exp

case class Value(a : Int) extends Exp

case class Div(l : Exp, r : Exp) extends Exp

// 4 / 5 ==> Div(Value(4), Value(5))

object Exp:
  def eval(e: Exp): Int = e match{
    case Value(i) => i
    case Div(l,r) => eval(l) / eval(r)
  }

  // Div(Value(2), Value(4))
  def evall(e: Exp): Option[Int] = e match {
    case Value(i) => Some(i)
    case Div(l ,r) => for{
      lv <- evall(l)
      rv <- evall(r)
    }yield lv/rv
  }

  def safeDivisin(a: Int, b: Int ): Option[Int] = if b == 0 then None else Some(a/b)

  def safeEval(e: Exp) : Option[Int] = e match{
    case Value(i) => Some(i)
    case Div(l ,r) => (safeEval(l), safeEval(r)) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(lv), Some(rv)) => Some(lv/rv)
    }
  }

  def saveEvalBetter(e: Exp) : Option[Int] = e match {
    case Value(i) => Some(i)
    case Div(l, r) => saveEvalBetter(l).flatMap(
      lv => saveEvalBetter(r).map(rv => lv / rv)
    )
  }
    
  

// a => Option[B], b => Option[C] --> a => Option[C]
