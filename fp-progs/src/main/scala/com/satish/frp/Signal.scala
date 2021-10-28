package com.satish.frp

class StackableVariable[T](init: T){
  var stackedValues = List(init)

  def value: T = stackedValues.head

  def withValue[R](currentValue : T)(exp: => R): R = {
    stackedValues = currentValue :: stackedValues

    try exp finally stackedValues = stackedValues.tail
  }
}


class Signal[T](exp: => T){
  import Signal._

  private var myExp: () => T = _

  private var myValue: T = _

  private var observers: Set[Signal[_]] = Set()

  update(exp)

  protected def update(exp: => T): Unit = {
    myExp = () => exp
    computeValue()
  }

  protected def computeValue(): Unit = {
    val newValue = caller.withValue(this){myExp()}
    if(newValue != myValue){
      myValue = newValue
      val obs = observers
      observers = Set()
      obs.foreach(ob => ob.computeValue())
    }
  }

  def apply(): T = {
    observers += caller.value
    assert(!caller.value.observers.contains(this), "cyclic signal definition")
    myValue
  }

  def numObs(): Int = observers.size


}

object NoSingal extends Signal[Nothing](???){

  override protected def computeValue(): Unit = ()

}

object Signal {
  private val caller = new StackableVariable[Signal[_]](NoSingal)
  def apply[T](exp: => T): Signal[T] = new Signal(exp)

}

class Var[T](exp: => T) extends Signal[T](exp){

  override def update(e: => T): Unit = super.update(e)

}

object Var{
  def apply[T](exp: => T): Var[T] = new Var(exp)
}
