package com.satish.examples.effect.pareffects
import java.util.concurrent.{Executor, Executors}
import scala.concurrent.ExecutionContext
import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.concurrent.duration.*

given ec:  ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

object Future1:
  def threadStr(s : String) : String = s"[${Thread.currentThread().getName}] $s"
  val hello = Future{
    println(threadStr("hello"))
    10
  }
  val world = Future{
    println(threadStr("world"))
    20
  }
  val sum: Future[Int] = hello.flatMap( x => world.map(y => x + y))





object FutureApp :
  import Future1.*
  final def main(args: Array[String]): Unit = {
    Thread.sleep(2000)

    println(hello)
    println(world)
    println(sum)

    Await.ready(sum, 5.second)
  }

