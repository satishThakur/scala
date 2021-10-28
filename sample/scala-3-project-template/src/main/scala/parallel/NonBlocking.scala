package parallel

import java.util.concurrent.{CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference

object NonBlocking:

  trait Future[+A]:
    private[parallel] def apply(cb: A => Unit): Unit

  type Par[+A] = ExecutorService => Future[A]

  object Par:

    def run[A](a: Par[A])(es: ExecutorService): A =
      val ref = AtomicReference[A]
      val cl = CountDownLatch(1)
      a(es){ aval => ref.set(aval);cl.countDown() }
      cl.await()
      ref.get

    def unit[A](a: A): Par[A] =
      es => new Future[A]:
        def apply(cb: A => Unit): Unit = cb(a)

    def fork[A](a: => Par[A]): Par[A] =
      es => new Future[A]:
        def apply(cb: A => Unit): Unit = eval(es)(a(es)(cb))

    def eval(es: ExecutorService)(s: => Unit) =
      es.submit(new Runnable {
        override def run(): Unit = s
      })

    //run a and b in parallel and then combine result
    def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
      es => new Future[C]:
        def apply(cb: C => Unit): Unit = {
          val refA = AtomicReference[A]
          val refB = AtomicReference[B]
          val cl = CountDownLatch(2)
          //compute a in separate thread - or schedule it.
          eval(es){
            a(es)(a => {
              refA.set(a)
              cl.countDown
            })
          }
          eval(es){
            b(es)(b => {
              refB.set(b)
              cl.countDown
            })
          }
          cl.await()
          cb(f(refA.get, refB.get))
        }


  end Par

end NonBlocking

