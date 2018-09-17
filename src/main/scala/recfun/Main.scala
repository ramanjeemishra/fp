package recfun

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
  def pascal(c: Int, r: Int): Int = (c, r) match {
    case (0, _) => 1
    case (x, y) if x > y => 0
    case _ => pascal(c - 1, r - 1) + pascal(c, r - 1)
  }


  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def loop(chars: List[Char], stack: List[Char]): List[Char] = {
      chars match {
        case Nil => stack
        case x :: cs if x == ')' && stack.nonEmpty => loop(cs, stack.tail)
        case x :: cs if x == ')' && stack.isEmpty => loop(cs, ')' :: stack)
        case x :: cs if x == '(' => loop(cs, '(' :: stack)
        case _ :: cs => loop(cs, stack)
        case x :: cs if x == ')' && stack.nonEmpty => loop(cs, stack.tail)
      }
    }

    loop(chars, Nil).isEmpty
  }


  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def go(money: Int, coins: List[Int]): Int = (money, coins) match {
      case (_, Nil) => 0
      case (0, _) => 0

    }

    0
  }
}
