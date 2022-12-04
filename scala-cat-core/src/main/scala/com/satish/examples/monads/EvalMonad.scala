package com.satish.examples.monads
import cats.Eval
object EvalMonad:

  def foldRight[A,B](ls: List[A], z: B)(f: (A, B) => B) : B = ls match{
    case x :: xs => f(x, foldRight(xs, z)(f))
    case Nil => z
  }

  def foldRightEval[A,B](ls: List[A], z: Eval[B])(f: (A,Eval[B]) => Eval[B]): Eval[B] = ls match{
    case Nil => z
    case x :: xs => Eval.defer(f(x, foldRightEval(xs, z)(f)))
  }

  def foldRightUsingEval[A,B](ls: List[A], z: B)(f: (A, B) => B) : B =
    foldRightEval(ls, Eval.now(z))((a, eb) => {
      eb.map(b => f(a,b))
    } ).value


object EvalMonadMain extends App:
  import cats.Eval

  val eager = Eval.now{
    println("eval.now")
    42
  }

  val always = Eval.always{
    println("eval.always")
    42
  }

  val later = Eval.later{
    println("eval.later")
    42
  }

  println(later.value)
  println(later.value)

  println("*********")

  val x = for{
    a <- Eval.now{println("i am here"); 5}
    b <- Eval.now{println("not here..."); 10}
  }yield (a + b)

  println("now printing value..")
  println(x.value)