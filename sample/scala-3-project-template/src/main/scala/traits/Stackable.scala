package traits

import traits.Stackable.queue

import scala.collection.mutable.ArrayBuffer

trait IntQueue:
  def put(v : Int): Unit
  def get: Int

class SimpleIntQueue extends IntQueue:
  val buffer = new ArrayBuffer[Int]()
  override def put(v: Int): Unit = buffer.append(v)
  override def get: Int = buffer.remove(0)

trait Doubler extends IntQueue:
  abstract override def put(v: Int): Unit = super.put(v * 2)

trait Incrementer extends IntQueue:
  abstract override def put(v: Int): Unit = super.put(v + 1)

trait Filter extends IntQueue:
  abstract override def put(v: Int): Unit = if v >= 0 then super.put(v)


object Stackable extends App:
  class MyQueue extends SimpleIntQueue with Filter with Incrementer
  val queue = new MyQueue
  queue.put(-1)
  queue.put(2)
  queue.put(3)

  println(queue.get)
  println(queue.get)


