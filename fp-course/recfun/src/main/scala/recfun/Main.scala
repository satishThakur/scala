package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println(balance("())(".toList))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r)
        1
      else
        pascal(c, r-1) + pascal(c- 1, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def count(c : Int, str : List[Char]) : Int = str match {
        case Nil => c
        case '(' :: xs => count(c + 1, xs)
        case ')' :: xs => if (c > 0) count(c -1, xs) else -1
        case _   :: xs => count(c,xs)

      }
      count(0,chars) == 0
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(money == 0){
        1
      }else if(money < 0 || coins.isEmpty){
        0
      }else{
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
      }
    }
  }
