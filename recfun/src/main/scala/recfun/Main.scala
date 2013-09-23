package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if(c == 0  || r == c)
      1
    else
      pascal(c-1, r-1) + pascal(c,r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean ={
    
    def matcher(chars: List[Char], matches: Int): Boolean ={
      if(matches < 0) return false
      
      if(chars.isEmpty) return matches == 0
      
      if(chars.head == '(') return matcher(chars.tail.toList, matches+1)
      
      if(chars.head == ')') return matcher(chars.tail.toList, matches-1)
      
      matcher(chars.tail.toList, matches)
    }
    
    matcher(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    3
  }
}
