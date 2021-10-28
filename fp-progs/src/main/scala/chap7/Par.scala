package chap7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}


object Par {
  //defines a deferred computation
  type Par[A] = ExecutorService => Future[A]

  //wrapper for already computed value
  def unit[A](a: A): Par[A] = _ => UnitFuture(a)



  //just a wrapper
  private case class UnitFuture[A](get: A) extends Future[A]{
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  //runs the computation by supplying es -
  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

  //7.3
  //just a wrapper
  private case class map2Future[A,B,C](fa: Future[A], fb: Future[B], f: (A,B) => C) extends Future[C]{

    //todo does volatile really work on Options - are they really atomic? This might not be correct
    @volatile var result: Option[C] = None

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = fa.isCancelled || fb.isCancelled

    override def isDone: Boolean = result.isDefined

    override def get(): C = compute(Long.MaxValue)

    override def get(timeout: Long, units: TimeUnit): C = compute(TimeUnit.NANOSECONDS.convert(timeout, units))

    private def compute(timeoutNs: Long): C = result match{
      case Some(x) => x
      case None => {
        val start = System.nanoTime()
        val a = fa.get(timeoutNs, TimeUnit.NANOSECONDS)

        val remainingTimeout = timeoutNs - (System.nanoTime() - start)
        val b = fb.get(remainingTimeout, TimeUnit.NANOSECONDS)
        f(a,b)
      }
    }
  }



  //combinator -
  def map2[A,B,C](p: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = es => {
    val fa = p(es)
    val fb = b(es)
    map2Future(fa, fb, f)
  }

  //what do we want in fork? - explicitly execute in different thread
  //es.submit will start the execution in threaf t1 which would do a(es).get - and  hence
  //t1 will block here - a(es) - might be happening in separate thread and hence 2 threads?
  def fork[A](a: =>Par[A]): Par[A] = es => es.submit(new Callable[A] {
    override def call(): A = a(es).get()
  })

  //intuition here-
  def lazyUnit[A](a: =>A): Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map2(parList, unit(List()))( (a,_) => a.sorted)


  def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(List()))( (a,_) => f(a))

  def sequence_simple[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List()))( (pa, pl) => map2(pa, pl)((_::_)))

  // We define `sequenceBalanced` using `IndexedSeq`, which provides an
  // efficient function for splitting the sequence in half.
  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l,r) = as.splitAt(as.length/2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork{
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      l map (asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten) // convenience method on `List` for concatenating a list of lists
  }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
  //if (run(es)(cond).get) t(es)
    es => if(run(es)(cond).get) t(es) else f(es)


  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val nv = run(es)(n).get()
      run(es)(choices(nv))
    }

  def choiceWithN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(x => if(x) 1 else 0))(List(t,f))

  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
    es => {
      val k = run(es)(key).get()
      run(es)(choices(k))
    }


  def flatMap[A,B](pa: Par[A])(f: A => Par[B]): Par[B] =
    es => {
      val a = run(es)(pa).get()
      run(es)(f(a))
    }

  def choiceUsingFlatMap[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    flatMap(cond)(b => if(b) t else f)

  def choiceNUsingFlatMap[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(n)(num => choices(num))

  def join[A](a: Par[Par[A]]): Par[A] =
    es => {
      run(es)(run(es)(a).get())
    }

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(a => a)

  def flatMapUsingJoin[A,B](pa: Par[A])(f: A => Par[B]): Par[B] =
    join(map(pa)(f))

}
