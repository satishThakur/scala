package parallel.week2

object PrefixSum {

  sealed abstract class Tree[A]

  case class Leaf[A](a: A) extends Tree[A]

  case class Node[A](l : Tree[A], r: Tree[A]) extends Tree[A]


  abstract class ResTree[A]{
    val res: A
  }

  case class ResLeaf[A](override val res: A) extends ResTree[A]

  case class ResNode[A](l: ResTree[A], override val res: A, r: ResTree[A]) extends ResTree[A]

  def upsweep[A](t: Tree[A], f: (A, A) => A): ResTree[A] = t match {
    case Leaf(a) => ResLeaf(a)
    case Node(l,r) => {
      val (lres, rres) = parallel( upsweep(l, f), upsweep(r, f))
      ResNode(lres, f(lres.res, rres.res), rres)
    }
  }


  def downsweep[A](r: ResTree[A], a0: A, f:(A,A) => A): Tree[A] = r match {
    case ResLeaf(res) => Leaf(f(a0, res))
    case ResNode(l, res, r) => {
      val (lt, rt) = parallel( downsweep(l, a0, f), downsweep(r, f(a0,l.res), f) )
      Node(lt, rt)
    }
  }

  def prepend[A](a0: A, t: Tree[A]): Tree[A] = t match{
    case Leaf(a) => Node(Leaf(a0), Leaf(a))
    case Node(l, r) => Node(prepend(a0, l), r)
  }

  def scanLeft[A](t: Tree[A],a0: A, f: (A, A) => A): Tree[A] = {
    prepend(a0, downsweep(upsweep(t, f), a0, f))
  }
}


