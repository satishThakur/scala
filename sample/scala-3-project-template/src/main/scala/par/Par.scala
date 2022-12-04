package par

import java.util.concurrent.{ExecutorService, Future, TimeUnit}

opaque type Par[A] = ExecutorService => Future[A]

object Par:
  extension[A](p: Par[A])
    def run(es: ExecutorService): Future[A] = p(es)

  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A]:
    override def isCancelled: Boolean = false
    override def isDone: Boolean = true
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    override def get(timeout: Long, unit: TimeUnit): A = get

  extension[A](pa : Par[A])
    def map2[B,C](pb : Par[B])(f : (A,B) => C) : Par[C] =
      es => {
        val fa = pa(es)
        val fb = pb(es)
        UnitFuture(f(fa.get(), fb.get()))
      }

  def fork[A](a : => Par[A]) : Par[A] =
    es => es.submit{
      () => a(es).get()
    }

  def lazyUnit[A](a : => A) : Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B) : A => Par[B] = a => lazyUnit(f(a))

  extension [A](pa: Par[A])
    def map[B](f: A => B): Par[B] =
      pa.map2(unit(()))((a, _) => f(a))

  def sequenceBalanced[A](pas: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
    if pas.isEmpty then unit(IndexedSeq.empty)
    else if pas.size == 1 then pas.head.map(a => IndexedSeq(a))
    else
      val (l, r) = pas.splitAt(pas.size / 2)
      sequenceBalanced(l).map2(sequenceBalanced(r))(_ ++ _)

  def sequence[A](pas: List[Par[A]]): Par[List[A]] =
    sequenceBalanced(pas.toIndexedSeq).map(_.toList)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val pars: List[Par[List[A]]] =
      l.map(asyncF(a => if f(a) then List(a) else List()))
    sequence(pars).map(_.flatten) // convenience method on `List` for concatenating a list of lists
  }