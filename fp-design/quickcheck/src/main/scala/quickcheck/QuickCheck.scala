package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {


  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(Gen.const(empty), genHeap)
  } yield insert(a, h)

/*
  lazy val genHeap: Gen[H] = oneOf(const(empty), for{
    i <- arbitrary[Int]
    h <- genHeap
  }yield insert(i,h))

 */

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  /**
    * If you insert any two elements into an empty heap,
    * finding the minimum of the resulting heap should get the smallest of the two elements back.
    */

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, empty)
    val h1 = insert(b, h)
    findMin(h1) == math.min(a,b)
  }

  /**
    * If you insert an element into an empty heap,
    * then delete the minimum, the resulting heap should be empty.
    */

  property("delmin1") = forAll { a: Int =>
    val h = insert(a, empty)
    val h2 = deleteMin(h)
    isEmpty(h2)
  }

  /**
    * Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
    *
    */
  property("meldmin") = forAll { (h1: H, h2: H) =>
    val h = meld(h1,h2)

    if(isEmpty(h1) && isEmpty(h2)) isEmpty(h)
    else{
      val m = findMin(h)
      if(isEmpty(h1)){
        m == findMin(h2)
      }else if(isEmpty(h2)){
        m == findMin(h1)
      }else {
        m == math.min(findMin(h1),findMin(h2))
      }
    }
  }

  /**
    * Given any heap, you should get a sorted sequence of elements
    * when continually finding and deleting minima.
    * (Hint: recursion and helper functions are your friends.)
    */

  property("deconstruct") = forAll { h1 : H =>
    def isSorted(heap: H): Boolean = {
      if(isEmpty(heap)) true
      else{
        val min = findMin(heap)
        val newHeap = deleteMin(heap)
        isEmpty(newHeap) || (min <= findMin(newHeap) && isSorted(newHeap))
      }
    }
    isSorted(h1)
  }

  // Take two arbitrary heaps, meld together. Then remove min from 1 and insert it into 2, meld the results. Compare two melds by comparing sequences of ranks.
  property("meld") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }
    if(isEmpty(h1) || isEmpty(h2)) true
    else
    heapEqual(meld(h1, h2),
      meld(deleteMin(h1), insert(findMin(h1), h2)))
  }

}
