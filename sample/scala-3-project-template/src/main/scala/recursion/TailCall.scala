package recursion

import scala.annotation.tailrec

class A:
  var b: B = null
  def setB(b : B) = this.b = b
  //@tailrec
  final def foo() : Unit =
    println("hello")
    b.foo()

class B:
  var a: A = null
  def setA(a : A) = this.a = a
  //@tailrec
  final def foo(): Unit =
    println("world")
    a.foo()



object TailCall extends App:

  @tailrec
  def func(): Unit =
    println("something")
    func()

  var x = 0
  def unsafeFunc(): Int =
    println("something-unsafe")
    x + unsafeFunc()

  //func()

  //unsafeFunc()

  val a = new A
  a.foo()