package com.satish.streams




class Pouring(capacity: Vector[Int]) {

  //state each glass is in -
  type State = Vector[Int]

  val initialState = capacity map(_ => 0)

  //moves
  trait Move{
    def change(s: State): State
  }

  case class Empty(glass: Int) extends Move{
    override def change(s: State): State = s.updated(glass, 0)
  }

  case class Pour(from: Int,to: Int) extends Move{
    override def change(s: State): State = {
      val transfer = math.min(s(from),capacity(to) - s(to))
      s updated(from, s(from) - transfer) updated(to, s(to) + transfer)
    }
  }

  case class Fill(glass: Int) extends Move{
    override def change(s: State): State = s updated(glass, capacity(glass))
  }


  val glasses = 0 until capacity.length

  def allMoves: IndexedSeq[Move] = {
    val emptyMoves = glasses map (g => Empty(g))

    val fillMoves = glasses map (g => Fill(g))

    val pourMoves = for{
      g1 <- glasses
      g2 <- glasses
      if(g1 != g2)
    }yield Pour(g1,g2)

    emptyMoves ++ fillMoves ++ pourMoves
  }

  class Path(history: List[Move]) {

    def endState: State = history.foldRight(initialState)(_ change _)

    def extend(m: Move): Path = new Path(m :: history)

    override def toString: String = history.reverse.mkString(" ") + " -->" + endState
  }


  val initalPath = new Path(Nil)


  def from(paths: Set[Path], visited: Set[State]): LazyList[Set[Path]] = {
    if(paths.isEmpty) LazyList.empty
    else{
      val next = for{
        path <- paths
        p <- allMoves.map(move => path.extend(move))
        if !visited.contains(p.endState)
      }yield p

      paths #:: from(next, (next map(p => p.endState)) ++ visited)
    }
  }

  val pathSets = from(Set(initalPath), Set(initialState))


  def solution(target: Int): LazyList[Path] = for{
    paths <- pathSets
    path <- paths
    if path.endState.contains(target)
  } yield path

}
