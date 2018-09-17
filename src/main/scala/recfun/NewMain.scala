package recfun

import scala.collection.mutable.Buffer
import scala.collection.mutable.ArrayBuffer

object NewMain {

  def generateArray(i: Int, j: Int) = {
    val arr1 = List.fill(i)(0).toBuffer
    val arr2 = List.fill(j)(0).toBuffer
    for (i1 <- 0 until i) {
      arr1(i1) = i1
    }
    for (j1 <- 0 until j) {
      arr2(j1) = j1
    }

    val arr3 = List[Buffer[Int]]().toBuffer

    for (i3 <- arr1) {
      for (j3 <- arr2)
        arr3 += List(i3, j3).toBuffer
    }

    arr3
  }

  def product(currentList: Buffer[Buffer[Int]], columnList: Buffer[Int]): Buffer[Buffer[Int]] = {
    val newList = List[Buffer[Int]]().toBuffer

    for (j <- columnList.indices) {
      for (i <- currentList.indices) {
        newList += ArrayBuffer(j) ++ currentList(i)
      }
    }
    newList
  }

  private def generateList(n: Int): Buffer[Int] = {
    val columnList = List.fill(n)(0).toBuffer
    for (i1 <- columnList.indices) {
      columnList(i1) = i1
    }
    columnList
  }

  def printAsMatrix(l2: Buffer[Buffer[Int]]): Unit = {
    l2.foreach(println(_))
  }

  def main(args: Array[String]): Unit = {
    val l3 = (0 until 5).map(Buffer(_)).toBuffer
    val l2 = product(product(l3, generateList(8)), generateList(9))
    println(l2.length)
    printAsMatrix(l2)

  }
}
