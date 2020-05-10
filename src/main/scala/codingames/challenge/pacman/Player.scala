//package codingames.challenge.pacman

import scala.io.Source
import scala.util._
import scala.io.StdIn._

object Direction extends Enumeration {
  type Direction = Value
  val North, East, South, West, NorthEast, NorthWest, SouthEast, SouthWest = Value
}
object Calc {

  import math._

  def euclidean(a: (Int, Int), b: (Int, Int)): Double = sqrt(pow(b._1 - a._1, 2) + pow(b._2 - a._2, 2))

  def euclidean(a: Square, b: Square): Double = sqrt(pow(b.x - a.x, 2) + pow(b.y - a.y, 2))
}
sealed abstract class Square(val x: Int, val y: Int) {
  def air: Boolean

  def rock: Boolean

  def sym: Char

  def index: Int

  var prise: Int = 0
  var free: Boolean = true
  var opp: Boolean = true
}
case class AirSquare(override val x: Int, override val y: Int, override val sym: Char, override val index: Int) extends Square(x, y) {
  override val air = true
  override val rock = false
}
case class RockSquare(override val x: Int, override val y: Int, override val sym: Char, override val index: Int) extends Square(x, y) {
  override val air = false
  override val rock = true
}
class Board(val squareMatrix: Array[Array[Square]]) {

  val transposedMatrix = transpose(squareMatrix)

  val toSet = squareMatrix.flatten.toSet

  def this(charData: Array[Array[Char]], air: Char, rock: Char) = {
    this {
      var index = 0
      charData.zipWithIndex.map(arrIndex => arrIndex._1.zipWithIndex.map(
        symIndex => {
          index += 1
          if (symIndex._1 == air)
            AirSquare(symIndex._2, arrIndex._2, symIndex._1, index) else
            RockSquare(symIndex._2, arrIndex._2, symIndex._1, index)
        })).asInstanceOf[Array[Array[Square]]]
    }
  }

  def this(rawData: List[String], air: Char, rock: Char) = {
    this(rawData.map(_.toCharArray).toArray, air, rock)
  }

  def apply(x: Int): Array[Square] = transposedMatrix(x)

  private def transpose(matrix: Array[Array[Square]]): Array[Array[Square]] = {
    matrix.head.indices.map(i => matrix.map(_ (i))).toArray
  }


  def show(squares: Set[Square]): Unit = {
    // Console.err.println(s"${toString(squares)}")
  }

  private def toString(squares: Set[Square]): String = {
    squareMatrix.map(row => row.map(s => if (squares.contains(s)) '\u25AE' else s.sym).mkString("")).mkString("\n")
  }

  override def toString: String = {
    squareMatrix.map(row => row.map(_.sym).mkString("")).mkString("\n")
  }

}
abstract class AbstractGame(board: Board) {

  import Direction._
  import scala.util._

  val cardinal = Set(North, South, West, East)
  val ordinal = Set(NorthWest, SouthWest, SouthEast, NorthEast)
  val windrose = cardinal ++ ordinal

  def takeRawSquareTry(x: Int, y: Int, direction: Direction, step: Int) = {
    direction match {
      case North => Try(board(x)(y - step))
      case South => Try(board(x)(y + step))
      case West => Try(board(x + step)(y))
      case East => Try(board(x - step)(y))
      case NorthWest => Try(board(x - step)(y - step))
      case SouthWest => Try(board(x - step)(y + step))
      case NorthEast => Try(board(x + step)(y - step))
      case SouthEast => Try(board(x + step)(y + step))
    }
  }
}
class Pac(val pacId: Int, val mine: Boolean, var x: Int, var y: Int, val typeId: String, var speedTurnsLeft: Int, var abilityCooldown: Int, var live: Boolean = true) {
  var target: Square = _
  var clash = false
  var targetType = 0      // 0 - max prise, shortest disatance,
                          // 1 - max prise, longest distance,
                          // 2 - min prise, shortest distance
                          // 3 - min prose, longest distance
  def command = s"MOVE ${pacId} ${target.x} ${target.y}"
  def reset = live = false; clash = false
}














object Player extends App {
  def linesToBoard(lines: List[String]): Board = new Board(lines, ' ', '#')
  def goodPac(pac: Pac) = pac != null && pac.live
  def needTarget(pac: Pac) = pac.target == null ||
    (pac.target.x == pac.x && pac.target.y == pac.y) ||
    pac.target.prise == 0 ||
    pac.clash

  def setTarget(pacs: Array[Pac], squares: Set[Square]) {
    var priseMap = squares.groupBy(_.prise) - 0

    val targetPacs = pacs.filter(pac => goodPac(pac) && needTarget(pac))
    val targetTypes = targetPacs.partition(_.clash)

    if (targetTypes._1.nonEmpty) {
      val minPac = targetTypes._1.minBy(_.pacId)
      minPac.targetType = (minPac.targetType + 1) % 4
    }

    for (pac <- pacs; if goodPac(pac) && needTarget(pac)) {
      pac.target = {
//        Console.err.println(s"${pac.pacId}")
        val maxPrise = priseMap.keySet.max
        val minPrise = priseMap.keySet.min
//        Console.err.println(s"\tprises: ${priseMap.keySet.mkString(",")}")
        var targetSet = priseMap(if (pac.targetType < 2) maxPrise else minPrise)
//        Console.err.println(s"\ttargetSet: $targetSet")
        val square = if (pac.targetType % 2 == 0) targetSet.minBy(square => Calc.euclidean((square.x, square.y), (pac.x, pac.y)))
                     else targetSet.maxBy(square => Calc.euclidean((square.x, square.y), (pac.x, pac.y)))
//        Console.err.println(s"\ttarget square: $square")
        targetSet = targetSet - square
        priseMap = if (targetSet.isEmpty) priseMap - maxPrise else priseMap + (maxPrise -> (targetSet))
        square
      }
    }
  }

  //------------------------------------------FILE ENTRY------------------------------------------------------------------
//        val filename = "resources/pacman/pacman2.txt"
//        val bufferedSource = Source.fromFile(filename)
//        val data = bufferedSource.getLines
//        def readInt = if (data.hasNext) data.next.toInt else -1
//        def readLine = if (data.hasNext) data.next else "EOF"
  //----------------------------------------------------------------------------------------------------------------------


  val Array(width, height) = (readLine split " ").map(_.toInt)
  // Console.err.println(s"${width} $height")
  val lines = (for (i <- 0 until height) yield readLine).toList
  val board = linesToBoard(lines)

  // Console.err.println(s"${board.toString}")

  val squares = board.toSet.filter(_.isInstanceOf[AirSquare])
  var visitedSquares = Set.empty[Square]
  var unvisitedSquares = squares
  var mypac = Array.fill[Pac](5)(null)
  var oppac = Array.fill[Pac](5)(null)

  while (true) {
    val Array(myScore, opponentScore) = (readLine split " ").map(_.toInt)
    // Console.err.println(s"$myScore $opponentScore")
    val visiblePacCount = readLine.toInt
    // Console.err.println(s"$visiblePacCount")

    mypac.filterNot(_ == null).foreach(_.reset)
    oppac.filterNot(_ == null).foreach(_.reset)

    for (i <- 0 until visiblePacCount) {
      val Array(_pacId, _mine, _x, _y, typeId, _speedTurnsLeft, _abilityCooldown) = readLine split " "
      val pacId = _pacId.toInt
      val mine = _mine.toInt != 0
      val x = _x.toInt
      val y = _y.toInt
      val speedTurnsLeft = _speedTurnsLeft.toInt
      val abilityCooldown = _abilityCooldown.toInt
      board(x)(y).prise = 0
      visitedSquares += board(x)(y)
      unvisitedSquares -= board(x)(y)

      val targetPac = if (mine) mypac else oppac
      if (targetPac(pacId) == null) targetPac(pacId) = new Pac(pacId, mine, x, y, typeId, speedTurnsLeft, abilityCooldown)
      else {
        targetPac(pacId).clash = targetPac(pacId).x == x && targetPac(pacId).y == y

        targetPac(pacId).x = x
        targetPac(pacId).y = y
        targetPac(pacId).speedTurnsLeft = speedTurnsLeft
        targetPac(pacId).abilityCooldown = abilityCooldown
        targetPac(pacId).live = true
      }
//       Console.err.println(s"${_pacId} $mine ${_x} ${_y} $typeId ${_speedTurnsLeft} ${_abilityCooldown}")
    }

    val visiblePelletCount = readLine.toInt
//     Console.err.println(s"$visiblePelletCount")
//    squares.foreach(_.prise = 0)
//     Console.err.println(s"<pellet...>")
    for (i <- 0 until visiblePelletCount) {
      val Array(x, y, value) = (readLine split " ").map(_.toInt)
      board(x)(y).prise = value
//       Console.err.println(s"$x $y $value")
    }

    setTarget(mypac, unvisitedSquares)

    val commands = mypac.collect {
      case pac if goodPac(pac) => pac.command
    }.mkString("|")

    println(commands)
  }
}