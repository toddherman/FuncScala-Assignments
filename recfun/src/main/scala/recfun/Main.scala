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
   * Take column c and row r
   */
  def pascal(c: Int, r: Int): Int = {
    // return 1 if 1st column or less; column gte row
    if (c <= 0 || c >= r) 1
    // recurse with previous column and row; PLUS 
    // recurse with current column and previous row
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   * verify the balancing of parentheses in a string
   */
  def balance(chars: List[Char]): Boolean = {
    if (chars.isEmpty) true
    else {
      //  inner function
      def checkNextChar(openCount: Int, ch: List[Char]): Int = {
        // if less than zero, return
        if (openCount < 0) -1
        // if there nothing left to check (the string has been exhausted), 
        // return count, which should eval to TRUE
        // given that above condition would have caught sub zero.
        else if (ch.isEmpty) openCount

        // is head an open paren?, recurse with next position on tail
        else if (ch.head == '(') checkNextChar(openCount + 1, ch.tail)
        // is head closing paren?, recurse with previous position on tail
        else if (ch.head == ')') checkNextChar(openCount - 1, ch.tail)

        // head was not paren, recurse with tail at the same index
        else checkNextChar(openCount, ch.tail)
      }
      // first call of inner, starting at zero position
      checkNextChar(0, chars) == 0
    }

    // alt
    //    @tailrec
    //    def balanced(chars: List[Char], open: Int): Boolean =
    //      chars match {
    //        case Nil => open == 0
    //        case '(' :: t => balanced(t, open + 1)
    //        case ')' :: t => open > 0 && balanced(t, open - 1)
    //        case _ :: t => balanced(t, open)
    //      }
    //    balanced(chars, 0)
  }

  /**
   * Exercise 3
   * number of ways you can make change, given a list of coin denominations
   */
  def countChange(money: Int, coins: List[Int]): Int = 
    // no money or coins, return zero
    if (money < 0 || coins.isEmpty) 0
    
    // if we've reached no more money, return 1
    else if (money == 0) 1
    
    // recurse with 2 calls added together
    // #1 amount of money minus first coin with list of all coins
    // #2 remaining money with remaining coins
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    
    
}
