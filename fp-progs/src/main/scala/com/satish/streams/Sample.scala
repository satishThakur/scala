package com.satish.streams

object Sample {


  def seive(nums: LazyList[Int]): LazyList[Int] =
    nums.head #:: seive(nums filter(_ % nums.head != 0))

  def main(args: Array[String]): Unit = {
    def isPrime(n: Int): Boolean = 2 until n forall(x => n % x != 0)

    val allPrimes = LazyList.from(10).filter(isPrime)

    println(allPrimes.take(10).toList)

    println(seive(LazyList.from(2)).take(10).toList)
  }


}
