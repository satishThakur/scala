package com.satish.combs

object Queens {

  def main(args: Array[String]): Unit = {
    println(queensWithMap(4))
    println(queens(4))
  }


  def queensWithMap(n : Int) : List[List[(Int, Int)]] = {

    def placeQueens(k : Int) : List[List[(Int,Int)]] = {

      if(k == 0) List(List())
      else{
        //solution till k-1th
        val qs = placeQueens(k-1)
        //lets process for kth row now - which is (K-1) => WE ARE Starting from 0.
        qs flatMap ( q => (0 until(n)).filter(col => safe(q, (k-1,col))).map(col => (k-1, col) :: q) )
      }
    }
    placeQueens(n)
  }



  //Look at the transformation from flatMap, Map and filter to for expression.
  def queens(n : Int) : List[List[(Int,Int)]] = {

    //place K queens in K Rows of board
    def placeQueens(k : Int) : List[List[(Int,Int)]] = {
      if (k == 0) List(List())
      else {
        for {
          q <- placeQueens(k - 1)
          col <- 0 until n
          if (safe(q, (k-1, col)))
        } yield (k-1, col) :: q
      }
    }
    //Place N queens on N rows of board
    placeQueens(n)
  }

  def safe(sol : List[(Int, Int)], point : (Int, Int)) : Boolean = {
    sol.forall(s => s._2 != point._2 && !isDiag(s, point))
  }

  //is queen diagnal to the pos - assume pos is on higher row than queen
  def isDiag(queen: (Int, Int), pos: (Int, Int)): Boolean ={
    val colDiff = math.abs(pos._2 - queen._2)
    pos._1 - queen._1 == colDiff
  }

}
