package parallel

import java.util.concurrent.{ExecutorService, Future, TimeUnit}



object Par {
  type Par[A] = ExecutorService => Future[A]
  def run[A](es: ExecutorService)(pa: Par[A]): Future[A] = pa(es)

  def unit[A](a : => A): Par[A] = _ => UnitFuture(a)

  private class UnitFuture[A](a: A) extends Future[A]{
    override def get(): A = a
    override def get(timeout: Long, unit: TimeUnit): A = a
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    override def isDone: Boolean = true
    override def isCancelled: Boolean = false
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def map2[A,B,C](a : Par[A], b : Par[B])(f: (A,B) => C): Par[C] =
    es => {
      val fa = a(es)
      val fb = b(es)
      //this is bit of a problem because UnitFuture is not actually a real future.
      //so a call on UnitFuture.get(timeout) will not work here as it will ignore the timeout.
      UnitFuture(f(fa.get, fb.get))
    }

  //why this method does not have same issue with map2?
  //Because we do not use our own UnitFuture here.
  //this fork submit a task - lets say that task executes in T1. All T1 does is passes same es to the
  //wrapped par and block waiting on result. If we use FixedThreadPool - this will deadlock.
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(() => a(es).get)

  //this will not block but it will do computation in same thread as well.
  //imagine implementing LazyUnit in this fork - the computation will end up in same thread.
  def delay[A](a: => Par[A]): Par[A] = es => a(es)

  //
  def asyncFunc[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  //there is nothing wrong with the implementation but it is better to use map2 -
  //map2 is more general than map
  def mapOld[A,B](a: Par[A])(f: A => B): Par[B] =
    es => {
      val fa = a(es)
      UnitFuture(f(fa.get))
    }

  //here unit(()) is unit(Unit) - The type does not matter as it is not used in f.
  //We could very well written unit(4)
  def map[A,B](a: Par[A])(f: A => B): Par[B] = map2(a, unit(()))((af, _) => f(af))

  //TODO why fork here?
  def parMap[A,B](ls: List[A])(f: A => B): Par[List[B]] = fork {
    val parB: List[Par[B]] = ls.map(asyncFunc(f))
    sequence(parB)
  }

  //Intution - given N parallel computition as a list - transform to a computation which produce
  //list of N computations
  def sequence[A](ls: List[Par[A]]): Par[List[A]] =
    ls match
      case Nil => unit(Nil)
      case x :: xs => map2(x, sequence(xs))(_::_)


  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      l map (asyncFunc((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten) // convenience method on `List` for concatenating a list of lists
  }

  //this is sequenciing - we get result of one Par and use that to choose..
  def choiceN[A](N : Par[Int])(pars: IndexedSeq[Par[A]]): Par[A] =
    es => {
      val nf = N(es)
      pars(nf.get)(es)
    }

  def choice[A](cnd: Par[Boolean])(para: Par[A], parb: Par[A]): Par[A] = {
    choiceN(map(cnd)(if _ then 0 else 1))(Vector(para, parb))
  }

  def choiseNWithFlatMap[A](N : Par[Int])(pars: IndexedSeq[Par[A]]): Par[A] =
    flatMap(N)(i => pars(i))


  //does this seem bit like state thingy - state guy used to pass state to
  //next execution - here we are passing es!!!
  def flatMap[A,B](parA: Par[A])(f: A => Par[B]): Par[B] = {
    //its sequencing - only way to get A is parA..
    es => {
      val parAf = parA(es)
      f(parAf.get)(es)
    }
  }

  def join[A](pa: Par[Par[A]]): Par[A] = flatMap(pa)(identity)

  //flatMap is mostly map and flatten - here flatten is join
  def flatMapUsingJoin[A,B](parA: Par[A])(f: A => Par[B]): Par[B] = join(map(parA)(f))


  def map2Future[A,B,C](a: Future[A], b: Future[B])(f: (A,B) => C): Future[C] = {
    @volatile var cache: Option[C] = None
    return new Future[C] {
      override def cancel(mayInterruptIfRunning: Boolean): Boolean =
        a.cancel(mayInterruptIfRunning) || b.cancel(mayInterruptIfRunning)

      override def isCancelled: Boolean = a.isCancelled || b.isCancelled

      override def isDone: Boolean = cache.isDefined

      override def get(): C = compute(Long.MaxValue)

      override def get(timeout: Long, unit: TimeUnit): C =
        compute(TimeUnit.MILLISECONDS.convert(timeout, unit))

      private def compute(toInMs: Long): C =
        cache match {
          case Some(c) => c
          case None => {
            val start = System.currentTimeMillis()
            val av = a.get(toInMs, TimeUnit.MILLISECONDS)
            val spentIna = System.currentTimeMillis() - start
            val bv = b.get(toInMs - spentIna, TimeUnit.MILLISECONDS)
            val cv = f(av, bv)
            cache = Some(cv)
            cv
          }
        }
    }
  }

  class MyConversion[A] extends Conversion[Par[A], ParOps[A]]:
    def apply(p : Par[A]): ParOps[A] = ParOps(p)

  class ParOps[A](p: Par[A]):
    def map[B](f: A => B): Par[B] = Par.map(p)(f)
    def map2[B,C](b: Par[B])(f: (A,B) => C): Par[C] = Par.map2(p,b)(f)
    def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(p)(f)

}
