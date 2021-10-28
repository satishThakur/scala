package reductions

import org.scalameter._

import scala.annotation.tailrec

object LineOfSightRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    println(s"parallel time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")
  }
}

sealed abstract class Tree {
  def maxPrevious: Float
}

case class Node(left: Tree, right: Tree) extends Tree {
  val maxPrevious = left.maxPrevious.max(right.maxPrevious)
}

case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree

object LineOfSight extends LineOfSightInterface {

  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    @tailrec
    def lineOfSight0(idx: Int, maxPrev: Float): Unit = {
      if(idx < input.length) {
        val newAngle = input(idx) / idx
        val newMax = math.max(maxPrev, newAngle)
        output(idx) = newMax
        lineOfSight0(idx + 1, newMax)
      }
    }
    output(0) = 0
    lineOfSight0(1, 0)
  }

  /** Traverses the specified part of the array and returns the maximum angle.
   */
  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {
    @tailrec
    def upsweepSeq0(idx: Int, preMax: Float): Float = {
      if(idx < until){
        val newAngle = input(idx)/idx
        upsweepSeq0(idx + 1, math.max(preMax, newAngle))

      }else preMax
    }
    upsweepSeq0(from, -1)
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
   *  returns the reduction tree for that part of the array.
   *
   *  The reduction tree is a `Leaf` if the length of the specified part of the
   *  array is smaller or equal to `threshold`, and a `Node` otherwise.
   *  If the specified part of the array is longer than `threshold`, then the
   *  work is divided and done recursively in parallel.
   */
  def upsweep(input: Array[Float], from: Int, end: Int,
    threshold: Int): Tree = {
    if( (end - from) <= threshold ){
      val max = upsweepSequential(input, from, end)
      Leaf(from, end, max)
    }else{
      val mid = (from + end)/2
      val (l,r) = parallel(upsweep(input, from, mid, threshold), upsweep(input, mid, end, threshold))
      Node(l,r)
    }
  }

  /** Traverses the part of the `input` array starting at `from` and until
   *  `until`, and computes the maximum angle for each entry of the output array,
   *  given the `startingAngle`.
   */
  def downsweepSequential(input: Array[Float], output: Array[Float],
    startingAngle: Float, from: Int, until: Int): Unit = {
    @tailrec
    def downsweepSequential0(idx: Int, maxAngle: Float): Unit = {
      if(idx < until){
        val currAngle = input(idx) / idx
        val max = math.max(maxAngle, currAngle)
        output(idx) = max
        downsweepSequential0(idx + 1, max)
      }
    }
    downsweepSequential0(from, startingAngle)
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
   *  reduction `tree` in parallel, and then calls `downsweepSequential` to write
   *  the `output` angles.
   */
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float,
    tree: Tree): Unit = {
    tree match {
      case Leaf(from, until, _) => downsweepSequential(input, output, startingAngle, from, until)
      case Node(l,r) => parallel(downsweep(input, output,startingAngle , l), downsweep(input, output,math.max(l.maxPrevious, startingAngle) , r))
    }
  }

  /** Compute the line-of-sight in parallel. */
  def parLineOfSight(input: Array[Float], output: Array[Float],
    threshold: Int): Unit = {
    val tree = upsweep(input, 1, input.length, threshold)
    downsweep(input, output, 0 , tree)
  }
}
