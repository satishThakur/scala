package variance

trait Queue[+T]:
    def enqueue[U >: T](elem: U): Queue[U]
    def head: T
    def tail: Queue[T]

object Queue:
  def apply[T](elems: T*): Queue[T] = new QueueImpl[T](elems.toList)

  private class QueueImpl[T](list: List[T]) extends Queue[T]:
    override def head: T = list.head

    override def tail: Queue[T] = new QueueImpl(list.tail)

    override def enqueue[U >: T](elem: U): Queue[U] = new QueueImpl[U](list ::: List(elem))

    override def toString: String = list.toString()


object QueueMain extends App:
  val myQueue = Queue(1,2,3,4)
  val newQueue = myQueue.enqueue(8)
  println(newQueue)