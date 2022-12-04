package com.satish.examples.monads
import cats.data.State
object PostOrderStateMonad:
  type CalcState[A] = State[List[Int], A]

  def evalInput(input: String): Int = evalAll(input.split(" ").toList).runA(Nil).value

  def evalAll(input: List[String]): CalcState[Int] = input match {
    case x :: Nil => evalOne(x)
    case x :: xs => evalOne(x).flatMap(_ => evalAll(xs))
  }

  def evalOne(sym: String): CalcState[Int] = sym match {
    case "+" => operator(_ + _)
    case "-" => operator(_ - _)
    case "*" => operator(_ * _)
    case _ => operand(sym.toInt)
  }

  def operand(opd: Int): CalcState[Int] =
    State[List[Int], Int]{
      state => (opd :: state, opd)
    }

  def operator(f: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int]{
      case a :: b :: xs =>
        val ans = f(a, b)
        (ans :: xs, ans)
      case _ => sys.error("fatal")
    }
object PostOrderMain extends App:
  import PostOrderStateMonad.*
  println(evalOne("42").runA(Nil).value)

  val program = for {
    _   <- evalOne("1")
    _   <- evalOne("2")
    ans <- evalOne("+")
  } yield ans

  println(program.runA(Nil).value)


  val sameProgram = evalAll(List("1", "2", "+"))
  println(sameProgram.runA(Nil).value)

  println(evalInput("5 2 *"))
