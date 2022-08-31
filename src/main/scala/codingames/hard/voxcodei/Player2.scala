package codingames.hard.voxcodei

import scala.io.Source
import scala.math._
import scala.util._
import scala.io.StdIn._

object Player2 extends App {
  //------------------------------------------FILE ENTRY------------------------------------------------------------------
  val filename = "resources/voxcodei/foresee-the-future-better.txt"
//  val filename = "resources/voxcodei/foresee-the-future.txt"
//  val filename = "resources/voxcodei/not-so-fast.txt"
//  val filename = "resources/voxcodei/destroy.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines
  def readInt = if (data.hasNext) data.next.toInt else { System.exit(0); -1 }
  def readLine = if (data.hasNext) data.next else { System.exit(0); "" }
  //----------------------------------------------------------------------------------------------------------------------

  type Matrix = Array[Array[Int]]
  var needToCalculate = true
  val AIR = 0
  val BOMB = 4
  val STONE = -2
  val TARGET = -1

  val Array(width, height) = (readLine split " ").withFilter(_ != "").map(_.toInt)
  Console.err.println(s"$width $height")

  def toMatrix(number: Int): (Int, Int) = (number % width, number / width)
  implicit def toNumber(point: (Int, Int)): Int = point._2 * width + point._1 % width

  var symMap = Map('.' -> AIR, '@' -> TARGET, '#' -> STONE)
  val lines = (for(_ <- 0 until height) yield readLine).toArray
  val initialMatrix = lines.flatMap(_.map(symMap(_)).toArray)

  def calculateSquare(xy: Int, matrix: Array[Int]) = {
    val step = 3
    var proceed = true
    val (x,y) = toMatrix(xy)
    val res1 = for (dy <- 1 to step; if y - dy >= 0; if matrix((x, y - dy)) < 0) yield {
      if (matrix((x, y - dy)) == STONE) {
        proceed = false
      }
      toNumber((x, y - dy))
    }
    proceed = true
    val res2 = for (dy <- 1 to step; if y + dy < height; if proceed; if matrix((x, y + dy)) < 0) yield {
      if (matrix((x, y + dy)) == STONE) {
        proceed = false
      }
      toNumber((x, y + dy))
    }
    proceed = true
    val res3 = for (dx <- 1 to step; if x - dx >= 0; if proceed; if matrix((x - dx, y)) < 0) yield {
      if (matrix((x - dx, y)) == STONE) {
        proceed = false
      }
      toNumber((x - dx, y))
    }
    proceed = true
    val res4 = for (dx <- 1 to step; if x + dx < width; if proceed; if matrix((x + dx, y)) < 0) yield {
      if (matrix((x + dx, y)) == STONE) {
        proceed = false
      }
      toNumber((x + dx, y))
    }
    (res1 ++ res2 ++ res3 ++ res4).filter(matrix(_) == TARGET).toList
  }

  def calculateSquares(matrix: Array[Int]) = {
    (for (square <- matrix.indices; if matrix(square) == AIR) yield {
      (square, calculateSquare(square, matrix))
    }).sortBy(_._2.length)(Ordering.Int.reverse)
  }

  def putBomb(to: Int, oldState: Array[Int]): Array[Int] = {
    val newState = oldState.clone()
    calculateSquare(to, newState).foreach(newState(_) = BOMB)
    newState(to) = BOMB
    newState
  }

  def newStates(oldState: Matrix): List[Matrix] = {
    ???
  }

  do {
    val Array(rounds, bombs) = (readLine split " ").withFilter(_ != "").map (_.toInt)
    if (needToCalculate) {
//      val res = calculateSquares(initialMatrix)
      needToCalculate = false
//      res.foreach(entry => Console.err.println(s"${entry._1}[${toMatrix(entry._1)}] -> ${entry._2}"))
//      Console.err.println(s"res=${res.toString}")
//      val oldaState = Array(1,2,3,4,5)
      val newState = putBomb(19, initialMatrix)
    }
//      println(newState)
  } while(true)
}