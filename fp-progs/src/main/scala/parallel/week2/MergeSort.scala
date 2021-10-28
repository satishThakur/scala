package parallel.week2

import java.util.{Arrays, Random}

import org.scalameter
import org.scalameter._

object MergeSort {

  def quickSort(arr: Array[Int], from: Int, to: Int): Unit = {
    java.util.Arrays.sort(arr, from ,to)
  }


  def main(args: Array[String]): Unit = {
    /*
      val a = Array(10,5,2, 11, 55, 33, 67, 89, 94, 12)
    //quickSort(a, 0, 3)

    mergeSort(a, 4)
    print(Arrays.toString(a))

     */

    val length = 10000000
    val maxDepth = 8

    val xs = new Array[Int](length)

    val seqTime = standardConfig setUp {
      _ => initialize(xs)
    } measure {

      quickSort(xs, 0, xs.length)
    }

    println("seq time - " + seqTime)

    val parTime = standardConfig setUp {
      _ => initialize(xs)
    } measure {
      mergeSort(xs, maxDepth)
    }

    println("Parallel time - " + parTime)



  }


  def mergeSort(xs: Array[Int], maxDepth: Int): Unit = {

    if(maxDepth %2 != 0) throw new IllegalArgumentException("mexdepth has to be multiple of 2")

    val ys = new Array[Int](xs.size)


    def merge(src: Array[Int], dest: Array[Int], from: Int, mid: Int, to : Int): Unit = {

      var s1Index = from
      var s2Index = mid
      var dstIndex = from
      while(s1Index < mid && s2Index < to){
        if(src(s1Index) < src(s2Index)){
          dest(dstIndex) = src(s1Index)
          s1Index = s1Index + 1
        }else{
          dest(dstIndex) = src(s2Index)
          s2Index = s2Index + 1
        }
        dstIndex = dstIndex + 1
      }

      while(s1Index < mid){
        dest(dstIndex) = src(s1Index)
        s1Index = s1Index + 1
        dstIndex = dstIndex + 1
      }

      while(s2Index < to){
        dest(dstIndex) = src(s2Index)
        s2Index = s2Index + 1
        dstIndex = dstIndex + 1
      }
    }

    def sort(from: Int, to: Int, depth: Int): Unit = {
      if(depth == maxDepth){
        quickSort(xs, from, to)
      }else{
        val mid = (from + to)/2
        parallel(sort(from, mid, depth + 1), sort(mid, to, depth + 1))

        val flip = (maxDepth - depth) % 2 == 0
        val src = if(flip) ys else xs
        val dest = if(flip) xs else ys
        merge(src, dest, from, mid, to)
      }

    }

    sort(0, xs.size, 0)

  }

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 60,
    Key.verbose -> true,
    Key.exec.benchRuns -> 60
  ).withWarmer(new scalameter.Warmer.Default)


  def initialize(xs: Array[Int]): Unit = {

    var index = 0
    val rand = new Random()

    while(index < xs.length){
      xs(index) = rand.nextInt(1000)
      index = index + 1
    }
  }

}
