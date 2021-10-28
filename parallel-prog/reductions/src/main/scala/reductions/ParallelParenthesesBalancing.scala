package reductions

import scala.annotation._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000
    val chars = new Array[Char](length)
    val threshold = 100
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balance0(c : Int, str : Array[Char]) : Boolean = {
      if(str.isEmpty) c == 0
      else{
        val ch = str.head
        if(ch == '(') balance0(c + 1, str.tail)
        else if(ch == ')'){
          if(c > 0) balance0(c -1, str.tail) else false
        }
        else balance0(c, str.tail)
      }
    }
    balance0(0,chars)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    @tailrec
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int,Int) = {
      if(idx == until) (arg1,arg2)
      else {
        val ch = chars(idx)
        if (ch == '(') traverse(idx + 1, until, arg1 + 1, arg2)
        else if (ch == ')') {
          if (arg1 > 0) traverse(idx + 1, until, arg1 - 1, arg2) else traverse(idx + 1, until, arg1, arg2 + 1)
        } else {
          traverse(idx + 1, until, arg1, arg2)
        }
      }
    }

    def reduce(from: Int, until: Int): (Int,Int) = {
      if(until - from <= threshold) traverse(from, until, 0, 0)
      else{
        val mid = (from + until) / 2
        val ((ll, lr), (rl, rr)) = parallel(reduce(from, mid), reduce(mid, until))
        val bal = math.min(ll, rr)
        (ll + rl - bal, lr + rr - bal)
      }
    }
    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
