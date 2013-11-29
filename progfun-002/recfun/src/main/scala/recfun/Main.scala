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
  def pascal(c: Int, r: Int): Int = (c, r) match {
    case (_, 0) | (0, _)  => 1
    case (x, y) if x == y => 1
    case (x, y)           => pascal(x - 1, y - 1) + pascal(x, y - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance(chars: List[Char], counter: Int): Boolean = {
      chars match {
        case Nil                       => counter == 0
        case '(' :: y                  => balance(y, counter + 1)
        case ')' :: y if (counter > 0) => balance(y, counter - 1)
        case ')' :: y                  => false
        case x :: y                    => balance(y, counter)
      }
    }
    balance(chars, 0)
  }
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def canBeGood(m: Int, coin: Int): Int =
      (m > 0 && m % coin == 0) match {
        case true  => 1
        case false => 0
      }

    def deroule(m: Int, coin: Int, nextCoins: List[Int]): Int = {
      if (coin - m <= 0) {
        nextCoins match {
          case Nil => canBeGood(m, coin)
          case x :: tail => {
            canBeGood(m, coin) + (for (i <- m.to(0, -coin)) yield deroule(i, nextCoins.head, nextCoins.tail)).toList.sum
          }
        }
      } else {
        0
      }
    }

    coins.sorted match {
      case Nil => 0
      case x   => deroule(money, x.head, x.tail)
    }
  }
}
