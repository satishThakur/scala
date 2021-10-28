package sample

object Sample extends App {

  def lazyFunc(expr: => String): Unit = {
    println("Invoked:: lazyFunc")
  }

  def lazyFuncForceEval(expr: => String): Unit = {
    println("Invoked:: lazyFuncForceEval")
    expr
  }

  def producer(): String = {
    println("Invoked:: producer")
    "something"
  }

  lazyFunc(producer())
  println()
  lazyFuncForceEval(producer())
}
