package recfun

import scala.annotation.tailrec

object FunctionalTest {


  private def abs(n: Int) = if (n < 0) -n else n

  private def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int = if (n == 0) acc else go(n - 1, n * acc)

    go(n, 1)
  }

  private def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, acc1: Int, acc: Int): Int = if (n == 1) acc1 else if (n == 2) acc else go(n - 1, acc, acc1 + acc)

    go(n, 0, 1)
  }

  def format(message: String, x: Int, f: Int => Int): String =
    s"$message ${f(x)}"

  def main(args: Array[String]): Unit = println(s" format factorial: ${format("factorial", 5, factorial)} , fib(5) : ${fib(5)}")
}
