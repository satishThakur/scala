package example
import parallel.Par.{Par, map2, unit, fork}

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

object ParallelExample extends App{

  def sum(ls : IndexedSeq[Int]): Par[Int] =
    if ls.isEmpty then unit(0) else
      if ls.length == 1 then unit(ls.head)
      else{
        val mid = ls.length/2
        val (l,r) = ls.splitAt(mid)
        map2(fork(sum(l)), fork(sum(r)))(_ + _)
      }

  val s = sum(1 to 10)

  val es = Executors.newCachedThreadPool()
  //val es = Executors.newSingleThreadExecutor()
  val v = s(es).get
  println(v)
  es.shutdown()

}
