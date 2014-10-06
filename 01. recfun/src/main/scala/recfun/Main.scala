package recfun
import common._

import scala.annotation.tailrec

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
    if (c == 0 || c == r) 1
    else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def checkBalance(chars: List[Char], stack: List[Char] = List()): Boolean = {
      if (chars.isEmpty) {
        stack.isEmpty
      } else if (chars.head == '(') {
        checkBalance(chars.tail, chars.head :: stack)
      } else if (chars.head == ')') {
        stack.nonEmpty && stack.head != chars.head && checkBalance(chars.tail, stack.dropRight(1)) // select all but the last
      } else {
        checkBalance(chars.tail, stack)
      }
    }
    checkBalance(chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else {
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
}
