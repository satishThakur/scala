package chap02

object Chap03 {


  def main(args: Array[String]): Unit = {

    //3.1

    //3.8

    val nums = (1 to 10).toList

    val l = MyList.foldRight(nums, Nil: List[Int])(_ :: _)

    println(l)




    println(MyList.take(nums, 4))

    println(MyList.takeWhile(nums)(x => x < 5))

    println(MyList.span(nums)(x => x < 5))

    println(MyList.init(nums))

    println(MyList.length(nums))
    println(MyList.length1(nums))

    println(MyList.reverse(nums))

    println(MyList.append(nums, nums))

    println(MyList.map(nums)(x => x * x))

    println(MyList.filter(nums)(x => x % 2 != 0))

    println(MyList.filterUsingFlatMap(nums)(x => x % 2 != 0))

    println(MyList.flatMap(nums)(x => List(x,x)))

    println(MyList.addList(nums,nums))

    println(MyList.hasSubsequence(List(1,2,3,4), List(2,3)))

    println(MyList.remove(nums,3))

    println(MyList.pack(List('a', 'a', 'a', 'b', 'b', 'c')))

    println(MyList.encode(List('a', 'a', 'a', 'b', 'b', 'c')))
  }


  object MyList {

    //3.1



    //3.2
    // TODO - Alternative could be where we return Optional -
    def tail[T](ls: List[T]): List[T] = ls match {
      case Nil => throw new NoSuchElementException("Nil.tail")
      case _ :: xs => xs
    }


    //3.3 - Immutable setHead-
    def setHead[T](ls: List[T], h: T): List[T] = ls match {
      case Nil => List(h)
      case _ :: xs => h :: xs
    }

    //3.4 -
    def drop[T](ls: List[T], n: Int): List[T] = ls match {
      case Nil => ls
      case _ :: xs => if (n == 0) ls else drop(xs, n - 1)
    }

    def take[T](ls : List[T], n : Int) : List[T] = ls match{
      case Nil => ls
      case x :: xs => if (n == 0) Nil else x :: take(xs, n -1)
    }

    //3.5 - These all examples shows how a persistence data structure is shared - we are not modifying the
    //entire list in any of the functions here..
    def dropWhile[T](ls: List[T])(f: T => Boolean): List[T] = ls match {
      case Nil => ls
      case x :: xs => if (f(x)) dropWhile(xs)(f) else xs
    }


    def takeWhile[T](ls: List[T])(f: T => Boolean): List[T] = ls match {
      case Nil => Nil
      case x :: xs => if ( f(x) ) x :: takeWhile(xs)(f) else Nil
    }


    def span[T](ls: List[T])(f: T => Boolean): (List[T], List[T]) = ls match {
      case Nil => (ls, ls)
      case x :: xs => if (f(x)) {
        val ns = span(xs)(f)
        (x :: ns._1, ns._2)
      }
      else (Nil, ls)
    }

    //3.6
    def init[T](ls: List[T]): List[T] = ls match {
      case Nil => Nil
      case _ :: Nil => Nil
      case x :: xs => x :: init(xs)
    }

    // f ( x, f(y, f(z, f(nil)) = f(x,f(y,f(z,zz

    //Why is this not tail recursive? because last call in the function f... needs
    //computation form further stacks so stack can not be wound here...
    def foldRight[T, U](ls: List[T], z: U)(f: (T, U) => U): U = ls match {
      case Nil => z
      case x :: xs => f(x, foldRight(xs, z)(f))
    }



    def sum(ls: List[Int]): Int = foldRight(ls, 0)(_ + _)

    def product(ls: List[Double]): Double = foldRight(ls, 1.0)(_ * _)

    //3.7 and 3.8 -->
    //3.7  - not in current form -

    //3.8

    //https://medium.com/@juntomioka/why-foldright-is-beautiful-7854ede3e133

    //3.9
    def length[T](ls: List[T]): Int = foldRight(ls, 0)((_, y) => y + 1)


    //3.10
    //We know that foldleft just keep calculating zero value and keep moving towards end
    @annotation.tailrec
    def foldLeft[T,U](ls: List[T], z: U)(f: (U, T) => U): U = ls match{
      case Nil => z
      case x :: xs => foldLeft(xs,f(z,x))(f)
    }


    def foldLeft123[T,U](ls: List[T], z: U)(f: (T, U) => U): U = ls match{
      case Nil => z
      case x :: xs => foldLeft123(xs,f(x,z))(f)
    }


    //above is better and readable form
    def foldLeft1[T, U](ls: List[T], z: U)(f: (U, T) => U): U = {
      @annotation.tailrec
      def loop(ls: List[T], acc: U): U = ls match {
        case Nil => acc
        case x :: xs => loop(xs, f(acc, x))
      }

      loop(ls, z)
    }

    //3.11

    //These are symetric function - as in - the Z type is same as T type - hence foldRight or left would work!!
    def sum1(ls: List[Int]): Int = foldLeft(ls, 0)(_ + _)

    def product1(ls: List[Double]): Double = foldLeft(ls, 1.0)(_ * _)

    def length1[T](ls: List[T]): Int = foldLeft(ls, 0)((x, _) => x + 1)


    //3.12

    def reverse[T](ls: List[T]): List[T] = foldLeft(ls, Nil: List[T])((xs, x) => x :: xs)

    //3.13

    //foldleft in terms of foldright ->

    def foldLeftUsingFoldRight[T,U](ls: List[T], z: U)(f: (U, T) => U): U = z //TODO


    //3.14

    def append[T](l: List[T], t: List[T]): List[T] = foldRight(l, t: List[T])((x, l) => x :: l)

    //3.15
    //this solution is not linear - but is quadratic ->
    def concat[T](l: List[List[T]]): List[T] = l match {
      case Nil => Nil
      case x :: List() => x
      case x :: y :: xs => concat(append(x, y) :: xs)
    }

    //define append via foldRight directly - This is linear to length of all lists and correct one.
    def concatWithFoldRight[T](l: List[List[T]]): List[T] = foldRight(l, Nil: List[T])(append)


    //3.16

    def transform(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((x, xs) => x + 1 :: xs)

    def transform1(l: List[Int]): List[String] = foldRight(l, Nil: List[String])((x, xs) => x.toString :: xs)


    def map[T, U](l: List[T])(f: T => U): List[U] = foldRight(l, Nil: List[U])((x, xs) => f(x) :: xs)

    def filter[T](l: List[T])(f: T => Boolean): List[T] =
      foldRight(l, Nil: List[T])((x, xs) => if (f(x)) xs else x :: xs)

    def flatMap[T, U](l: List[T])(f: T => List[U]): List[U] =
      foldRight(l, Nil: List[U])((x, xs) => append(f(x), xs))

    def filterUsingFlatMap[T](l: List[T])(f: T => Boolean): List[T] =
      flatMap(l)(x => if (f(x)) Nil else List(x))

    def addList(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (x :: xs, p :: px) => (x + p) :: addList(xs, px)
    }

    def zipWith[T, R, U](l: List[T], r: List[R])(f: (T, R) => U) : List[U] = (l, r) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (x :: xs, p :: px) => f(x,p) :: zipWith(xs, px)(f)
    }

    //3.24
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (_ :: sx, _) => if (isSub(sup, sub)) true else hasSubsequence(sx, sub)
    }

    def isSub[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (s :: sx, p :: px) => if (s == p) isSub(sx, px) else false
    }


    // extra stuff....


    def remove[T](l : List[T], n : Int) : List[T] = l match{
      case List() => l
      case x :: xs => if (n == 0) xs else x :: remove(xs, n-1)
    }

    //merge part of merge sort algo - can be generalized -
    def merge(xs : List[Int], ys : List[Int]) : List[Int] = (xs, ys) match{
      case (_, List()) => xs
      case (List(), _) => ys
      case (x::xs1, y :: ys1) => if(x < y) x :: merge(xs1, ys) else y :: merge(xs, ys1)
    }

    def pack[T](l : List[T]) : List[List[T]] = l match{
      case Nil => Nil
      case x :: _ =>
        val (first,last) = l.span(y => y == x)
        first :: pack(last)
    }

    def encode[T](l : List[T]) : List[(T, Int)] = pack(l) map (l => (l.head, l.size))


  }


}