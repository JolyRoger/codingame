package codingames.hard.voxcodei

import java.util
import scala.collection.mutable
import scala.io.Source
import scala.math._
import scala.util._
import scala.io.StdIn._

object Player extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
// val filename = "resources/voxcodei/one-node.txt"
  val filename = "resources/voxcodei/foresee-the-future-better.txt"
// val filename = "resources/voxcodei/foresee-the-future.txt"
// val filename = "resources/voxcodei/not-so-fast.txt"
// val filename = "resources/voxcodei/destroy.txt"
 val bufferedSource = Source.fromFile(filename)
 val data = bufferedSource.getLines
 def readInt = if (data.hasNext) data.next.toInt else { System.exit(0); -1 }
 def readLine = if (data.hasNext) data.next else { System.exit(0); "" }
//----------------------------------------------------------------------------------------------------------------------

  type Field = Array[Int]
  type State = (Field, Int, Int)    // field, rounds, bombs
  type Link = (Int, Int)            // from, prev state hash
  type LinkedState = (State, Link)  // state, link

  var needToCalculate = true
  val AIR = 0
  val BOMB = 4
  val STONE = -2
  val TARGET = -1
  val WAIT = -1
  val NO_VALUE = Int.MinValue
  val Array(width, height) = (readLine split " ").withFilter(_ != "").map(_.toInt)

  def toMatrix(number: Int): (Int, Int) = (number % width, number / width)
  implicit def toNumber(point: (Int, Int)): Int = point._2 * width + point._1 % width

  val symMap = Map('.' -> AIR, '@' -> TARGET, '#' -> STONE)
  val lines = (for(_ <- 0 until height) yield readLine).toArray
  val initialMatrix = lines.flatMap(_.map(symMap(_)).toArray)
  val dataMap = mutable.Map.empty[Int, Link]
  val consumedStates = mutable.Set.empty[Int]

  def calculateSquare(xy: Int, matrix: Field) = {
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

  def calculateSquares(matrix: Field) = {
    (for (square <- matrix.indices; if matrix(square) == AIR) yield {
      (square, calculateSquare(square, matrix))
    }).sortBy(_._2.length)(Ordering.Int.reverse)
  }

  def tick(oldState: Field): Field = oldState.map(square => if (square > AIR && square <= BOMB) square - 1 else square)

  def putBomb(to: Int, matrix: Field): Field = {
    val newState = matrix.clone()
    calculateSquare(to, newState).foreach(newState(_) = BOMB)
    newState(to) = BOMB
    newState
  }

  def newStates(oldState: LinkedState): List[LinkedState] = {
    val (state, link) = oldState
    val (field, rounds, bombs) = state

    val dataHash = oldState.hashCode()
    val stateHash = util.Arrays.hashCode(oldState._1._1.zipWithIndex.withFilter(_._1 == -1).map(_._2))
    dataMap.put(dataHash, oldState._2)

    val out = if (rounds == 0) {
      List.empty
    } else if (consumedStates.contains(stateHash) && link._1 != WAIT) {
      List.empty
    } else if (bombs == 0) {
      List(((tick(field), rounds - 1, bombs), (WAIT, dataHash)))
    } else {
      (calculateSquares(field).filter(_._2.nonEmpty).map(bomb => ((tick(putBomb(bomb._1, field)), rounds - 1, bombs - 1), (bomb._1, dataHash))).toList) :::
        List(((tick(field), rounds - 1, bombs), (WAIT, dataHash)))
    }
    consumedStates.add(stateHash)
    out
  }

  def dirty(field: Field) = field.contains(TARGET)

  def calculate(from: LinkedState) = {
    var stack = List(from)
    var searching = true
    var current: LinkedState = from
    dataMap.put(from.hashCode(), from._2)

    while (stack.nonEmpty && searching) {
      current = stack.head
      searching = dirty(current._1._1)
      if (searching) {
        stack = stack.tail
        val nextStates = newStates(current)
        stack = nextStates ::: stack
      }
    }

    var result = List.empty[Int]
    var currentLink = current._2

    while (currentLink._1 != NO_VALUE) {
      result =  currentLink._1 :: result
      currentLink = dataMap(currentLink._2)
    }
    result
  }

  var moves = List.empty[Int]

  do {
    val Array(rounds, bombs) = (readLine split " ").withFilter(_ != "").map (_.toInt)

    moves = if (needToCalculate) {
      val initialData = ((initialMatrix, rounds, bombs), (NO_VALUE, NO_VALUE))
      needToCalculate = false
      calculate(initialData)
    } else if (moves.isEmpty) moves
    else moves.tail

    println {
      moves.headOption.map { move =>
        if (move == -1) "WAIT" else {
          val (x, y) = toMatrix(move)
          s"$x $y"
        }
      }.getOrElse("WAIT")
    }
  } while(true)
}
