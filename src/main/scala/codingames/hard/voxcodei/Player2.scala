package codingames.hard.voxcodei

import scala.collection.mutable
import scala.io.Source
import scala.math._
import scala.util._
import scala.io.StdIn._

object Player2 extends App {
  //------------------------------------------FILE ENTRY------------------------------------------------------------------
//  val filename = "resources/voxcodei/foresee-the-future-better.txt"
  val filename = "resources/voxcodei/foresee-the-future.txt"
//  val filename = "resources/voxcodei/not-so-fast.txt"
//  val filename = "resources/voxcodei/destroy.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines
  def readInt = if (data.hasNext) data.next.toInt else { System.exit(0); -1 }
  def readLine = if (data.hasNext) data.next else { System.exit(0); "" }
  //----------------------------------------------------------------------------------------------------------------------

  type Field = Array[Int]
  type Data = (Field, Int, Int, Int, Int) // field, rounds, bombs, from, prev state hash

  var needToCalculate = true
  val AIR = 0
  val BOMB = 4
  val STONE = -2
  val TARGET = -1
  val WAIT = -1
  val NO_VALUE = Int.MinValue

  val Array(width, height) = (readLine split " ").withFilter(_ != "").map(_.toInt)
  Console.err.println(s"$width $height")

  def toMatrix(number: Int): (Int, Int) = (number % width, number / width)
  implicit def toNumber(point: (Int, Int)): Int = point._2 * width + point._1 % width

  val symMap = Map('.' -> AIR, '@' -> TARGET, '#' -> STONE)
  val lines = (for(_ <- 0 until height) yield readLine).toArray
  val initialMatrix = lines.flatMap(_.map(symMap(_)).toArray)
  val dataMap = mutable.Map.empty[Int, Data]

  def print(matrix: Field): Unit = {
    for (i <- matrix.indices) {
      if (i % width == 0) Console.err.println
      val sym = if (matrix(i) == AIR) "."
                else if (matrix(i) == STONE) "*"
                else if (matrix(i) == TARGET) "T"
                else matrix(i).toString
      Console.err.print(s"$sym")
    }
  }

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

  def newStates(oldState: Data): List[Data] = {
    val (field, rounds, bombs, _, _) = oldState
    if (rounds == 0) {
      List.empty
    } else if (bombs == 0) {
      List((tick(field), rounds - 1, bombs, WAIT, oldState.hashCode()))
    } else {
      val squares = calculateSquares(field).filter(_._2.nonEmpty)
      val hash = oldState.hashCode()
      val newStates = squares.map(bomb => (tick(putBomb(bomb._1, field)), rounds - 1, bombs - 1, bomb._1, hash)).toList
      (tick(field), rounds - 1, bombs, WAIT, hash) :: newStates
    }
  }

  def dirty(field: Field) = field.contains(TARGET)

  def calculate(from: Data) = {
    var stack = List(from)
    var searching = true
    var current: Data = from
    dataMap.put(from.hashCode(), from)

    while (stack.nonEmpty && searching) {
      current = stack.head
      searching = dirty(current._1)
      if (searching) {
        stack = stack.tail
        val nextStates = newStates(current)
        nextStates.foreach(state => dataMap.put(state.hashCode(), state))
        stack = nextStates ::: stack
      }
    }

    var result = List.empty[Int]

    while (current._4 != NO_VALUE) {
      result = current._4 :: result
      current = dataMap(current._5)
    }
    result
  }

  do {
    val Array(rounds, bombs) = (readLine split " ").withFilter(_ != "").map (_.toInt)
    if (needToCalculate) {
//      val res = calculateSquares(initialMatrix)
      val initialData = (initialMatrix, rounds, bombs, NO_VALUE, NO_VALUE)
      val res = calculate(initialData)
//      val res = newStates(initialData)
      needToCalculate = false
      Console.err.println(s"res=$res")
//      res.foreach(r => {
//        Console.err.println(s"=========================")
//        Console.err.println(s"${print(r._1)}")
//        Console.err.println(s"round=${r._2} bombs=${r._3} from=(${toMatrix(r._4)})")
//      })
//      res.foreach(entry => Console.err.println(s"${entry._1}[${toMatrix(entry._1)}] -> ${entry._2}"))
//      Console.err.println(s"res=${res.toString}")
//      val oldaState = Array(1,2,3,4,5)
//      print(initialMatrix)
//      val newState = putBomb(82, initialMatrix)
//      Console.err.println
//      Console.err.print(s"============================")
//      print(newState)
//      val tickState = tick(newState)
//      Console.err.println
//      Console.err.print(s"TICK")
//      print(tickState)
//      val tackState = tick(tickState)
//      Console.err.println
//      Console.err.print(s"TACK")
//      print(tackState)
    }
  } while(true)
}