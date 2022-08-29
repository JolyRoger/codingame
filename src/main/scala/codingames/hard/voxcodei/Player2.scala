package codingames.hard.voxcodei

import scala.io.Source
import scala.math._
import scala.util._
import scala.io.StdIn._

object Player2 extends App {
  //------------------------------------------FILE ENTRY------------------------------------------------------------------
//    val filename = "resources/voxcodei/foresee-the-future-better.txt"
  //  val filename = "resources/voxcodei/foresee-the-future.txt"
//  val filename = "resources/voxcodei/not-so-fast.txt"
    val filename = "resources/voxcodei/destroy.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines
  def readInt = if (data.hasNext) data.next.toInt else { System.exit(0); -1 }
  def readLine = if (data.hasNext) data.next else { System.exit(0); "" }
  //----------------------------------------------------------------------------------------------------------------------

  type Matrix = List[List[Int]]
  var needToCalculate = true
  val AIR = 0
  val STONE = -2
  val TARGET = -1

  val Array(width, height) = (readLine split " ").filter(_ != "").map (_.toInt)
  Console.err.println(s"$width $height")

  def toMatrix(number: Int): (Int, Int) = (number % width, number / width)
  def toNumber(point: (Int, Int)): Int = point._2 * width + point._1 % width

  var symMap = Map('.' -> AIR, '@' -> TARGET, '#' -> STONE)
  val lines = (for(_ <- 0 until height) yield readLine).toList
  val initialMatrix = lines.map(_.map(symMap(_)).toList)
  val initialArray = initialMatrix.flatten

  def calculateSquare(x: Int, y: Int, matrix: Matrix) = {
    val step = 3
    var proceed = true
    val res1 = for (dy <- 1 to step; if y - dy >= 0; if matrix(y - dy)(x) < 0) yield {
      if (matrix(y - dy)(x) == STONE) {
        proceed = false
      }
      (x, y - dy)
    }
    proceed = true
    val res2 = for (dy <- 1 to step; if y + dy < height; if proceed; if matrix(y + dy)(x) < 0) yield {
      if (matrix(y + dy)(x) == STONE) {
        proceed = false
      }
      (x, y + dy)
    }
    proceed = true
    val res3 = for (dx <- 1 to step; if x - dx >= 0; if proceed; if matrix(y)(x - dx) < 0) yield {
      if (matrix(y)(x - dx) == STONE) {
        proceed = false
      }
      (x - dx, y)
    }
    proceed = true
    val res4 = for (dx <- 1 to step; if x + dx < width; if proceed; if matrix(y)(x + dx) < 0) yield {
      if (matrix(y)(x + dx) == STONE) {
        proceed = false
      }
      (x + dx, y)
    }
    (res1 ++ res2 ++ res3 ++ res4).toList.filter(xy => matrix(xy._2)(xy._1) == TARGET)
  }

  def calculateSquares(matrix: Matrix) = {
    (for (y <- matrix.indices; x <- matrix.head.indices; if (matrix(y)(x) == AIR)) yield {
      ((x,y), calculateSquare(x, y, matrix))
    }).sortBy(_._2.length)(Ordering.Int.reverse)
  }

  def newStates(oldState: Matrix): List[Matrix] = {
    ???
  }

  do {
    val Array(rounds, bombs) = (readLine split " ").filter(_ != "").map (_.toInt)
    if (needToCalculate) {
      val res = calculateSquares(initialMatrix)
      needToCalculate = false
      res.foreach(entry => Console.err.println(s"${entry._1} -> ${entry._2}"))
      Console.err.println(s"res=${res.toString}")
    }
//    println("WAIT")
  } while(true)
}