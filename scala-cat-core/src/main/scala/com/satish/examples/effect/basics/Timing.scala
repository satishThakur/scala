package com.satish.examples.effect.basics

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

object Timing extends App :
  val clock: MyIO[Long] = MyIO(() => System.currentTimeMillis())

  def time[A](action: MyIO[A]): MyIO[(FiniteDuration, A)] =
    for {
      start <- clock
      result <- action
      end <- clock
    } yield (FiniteDuration(end - start, TimeUnit.MILLISECONDS), result)

  val timedHello = Timing.time(Console.write("hello"))
  timedHello.runUnsafe() match {
    case (duration, _) => println(s"'hello' took $duration")
  }
