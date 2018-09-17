package recfun

import java.lang.System.{currentTimeMillis, nanoTime}

import scala.Int.MaxValue
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Buffer}

/** **
  * DPChange(money , coins )
  * MinNumCoins(0) ← 0
  * for m from 1 to money:
  * MinNumCoins(m) ← ∞
  * for i from 1 to |coins|:
  * if m≥coini:
  * NumCoins ← MinNumCoins (m − coini ) + 1
  * if NumCoins < MinNumCoins(m):
  * MinNumCoins(m) ← NumCoins
  * return MinNumCoins(money)
  */
object MinCoins {

  def main(args: Array[String]): Unit = {
    val ns = currentTimeMillis()
    //    println(s"findTotalRestrictedOptions ${findTotalRestrictedOptions(300, List(5, 10, 20, 50, 100, 200, 500))} Time : ${currentTimeMillis() - ns}")
    val ns1 = currentTimeMillis()
    //    println(s"findTotalRestrictedOptions2 ${findTotalRestrictedOptions2(300, List(5, 10, 20, 50, 100, 200, 500))}  Time : ${currentTimeMillis() - ns1}")
    println(s"change coins : ${change(300, List(50, 10, 20, 500, 100, 200, 5)).length}  Time : ${currentTimeMillis() - ns1}")
    //println(s"change coins ${change(4, List(1, 2))}  Time : ${currentTimeMillis() - ns1}")
  }


  def minChangeCount(money: Int, coins: List[Int]): Int = {
    val minCoins = List.fill(money + 1)(0).toBuffer
    for (m <- 1 to money) {
      var numCoins = 0
      minCoins(m) = MaxValue
      for (coin <- coins) {
        if (m >= coin) {
          numCoins = minCoins(m - coin) + 1
          if (numCoins < minCoins(m)) {
            minCoins(m) = numCoins
          }
        }
      }
    }
    minCoins(money)
  }

  def allChangeCount(money: Int, coins: List[Int]): Int = {
    val allChange = List.fill(money + 1)(0).toBuffer
    for (m <- 1 to money) {
      for (coin <- coins) {
        println(s"money: $m with coin: $coin")
        if (m >= coin) {
          allChange(m) += allChange(m - coin) + 1
          println(s"minCoins($m) : ${allChange(m)}")
        }
      }
    }
    allChange(money)
  }


  def findTotalRestrictedOptions(n: Int, coins: List[Int]): Int = {
    var totalOptions: Int = 0
    for (a <- 0 to n / 5;
         c <- 0 to n / 20;
         d <- 0 to n / 50;
         e <- 0 to n / 100;
         f <- 0 to n / 200;
         b <- 0 to n / 10;
         g <- 0 to n / 500
         if 5 * a + 10 * b + 20 * c + 50 * d + 100 * e + 200 * f + 500 * g == n) {
      totalOptions += 1
      //println(s"totalOptions :  a: $a, b : $b, c:$c, d:$d, e:$e, f:$f, g:$g opt: $totalOptions")
    }

    totalOptions
  }

  def findTotalRestrictedOptions2(n: Int, coins: List[Int]): Int = {
    val maxNoOfCoins: List[Int] = coins.map(n / _)

    var l3 = List(List[Int]())
    for (i <- maxNoOfCoins.length - 1 to 0 by -1) {
      l3 = if (i != 0) product(l3, maxNoOfCoins(i), { _ => true })
      else product(l3, maxNoOfCoins(i), { l => (l zip coins).map { case (x, y) => x * y }.sum == n })
    }
    l3.size
  }

  def product(currentList: List[List[Int]], n: Int, p: List[Int] => Boolean) = {
    var newList: List[List[Int]] = List[List[Int]]()
    val ms = currentTimeMillis()
    val a2 = currentList.toArray
    println(s" time taken for itr ${n} - ${currentTimeMillis() - ms}")
    val ms1 = currentTimeMillis()
    for (i <- 0 to n) {
      for (j <- a2.indices) {
        val l1 = i :: a2(j)
        newList = l1 :: newList
        /*if (p(l1)) {

        }*/
      }
    }
    println(s" time taken for itr ${n} -  ${currentTimeMillis() - ms1}")
    newList
  }


  def changeCoins(money: Int, coins: List[Int]): List[Any] = {
    println(s"money : $money, coins :$coins")
    (money, coins) match {
      case (0, _) =>
        println(s"Nil-1")
        List(0)
      case (_, Nil) =>
        println(s"Nil-2")
        Nil
      case (m, hd :: Nil) if m - 1 + hd == m => {
        val l = changeCoins(m - 1, hd :: Nil)
        println(s"==> m: $m, hd :$hd,   l : $l")
        hd :: l
      }
      // case (m, hd :: Nil) if m % hd == 0 => List.fill(m / hd)(hd) :: Nil
      case (m, hd :: Nil) if m - 1 + hd >= m => Nil
      case (m, lst) => changeCoins(m, lst.head :: Nil) :: changeCoins(m, lst.tail) //:: changeCoins(m - 1, lst)

    }
  }

  def change(money: Int, coins: List[Int]): List[List[Int]] = {
    def go(money: Int, coins: List[Int], solCache: Array[List[List[Int]]]): List[List[Int]] = {
      // println(s"($money)")
      (money, coins) match {
        case (_, Nil) => Nil
        case (0, _) => List(List(0))

        case (m, cs) => {
          if (solCache(m) != Nil) solCache(m)
          else {
            val solutions: List[List[List[Int]]] = for (c <- cs; if m - c >= 0) yield go(m - c, cs, solCache)
            val l1 = for (c <- cs; sols <- solutions; sol <- sols; if (c :: sol).sum == m) yield (c :: sol).sorted
            println(s"$money :=> ${l1.length}  : ${l1.distinct.length}")
            solCache(m) = l1.distinct
            l1.distinct
          }
        }
      }
    }

    go(money, coins, List.fill(money + 1)(Nil).toArray)
  }


}
