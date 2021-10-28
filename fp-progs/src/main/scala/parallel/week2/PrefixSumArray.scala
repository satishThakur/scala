package parallel.week2

import java.util


object PrefixSumArray {

  def main(args: Array[String]): Unit = {

    val a = Array(1,2,3,4,5)
    val o = new Array[Int](a.length + 1)
    scanLeft[Int](a, 100, (_ + _), o)
    println(util.Arrays.toString(a))
    println(util.Arrays.toString(o))
  }

  abstract class ResTreeA[A]{
    val res: A
  }

  case class ResLeafA[A](from:Int, to: Int, override val res: A) extends ResTreeA[A]

  case class ResNodeA[A](l: ResTreeA[A], override val res: A, r: ResTreeA[A]) extends ResTreeA[A]

  def upsweep[A](t: Array[A], from: Int, to: Int, threshold: Int, f: (A, A) => A): ResTreeA[A] = {
    if (to - from <= threshold) {
      var r = t(from)
      var idx = from + 1
      while (idx < to) {
        r = f(r, t(idx))
        idx = idx + 1
      }
      ResLeafA(from, to, r)
    } else {
      val mid = (from + to) / 2
      val (l, r) = (upsweep(t, from, mid, threshold, f), upsweep(t, mid, to, threshold, f))
      ResNodeA(l, f(l.res, r.res), r)
    }
  }


  def downsweep[A](input: Array[A], rt: ResTreeA[A], a0: A, f: (A, A) => A, output: Array[A]): Unit = rt match {
    case ResLeafA(from, to, res) => {
      var idx = from
      var res = a0
      while (idx < to) {
        res= f(res, input(idx))
        idx = idx + 1
        output(idx) = res
      }
    }
    case ResNodeA(l, _, r) => {
      downsweep(input, l, a0, f, output)
      downsweep(input, r, f(a0, l.res), f, output)

    }
  }

  def scanLeft[A](input: Array[A], a0: A, f: (A, A) => A, output:Array[A]): Unit = {
    val u = upsweep(input, 0, input.length, 4, f)
    downsweep(input, u, a0, f, output)
    output(0) = a0
  }

}


