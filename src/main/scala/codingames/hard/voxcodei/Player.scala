package codingames.hard.voxcodei

import scala.io.Source
import scala.math._
import scala.util._

object Calc {
  def euclidean(a: (Int, Int), b: (Int, Int)): Double = sqrt(pow(b._1 - a._1, 2) + pow(b._2 - a._2, 2))
  def euclidean(a: Square, b: Square): Double = sqrt(pow(b.x - a.x, 2) + pow(b.y - a.y, 2))
}
object SquareType extends Enumeration {
  type SquareType = Value
  val Air, Rock, Other = Value
}
object Direction extends Enumeration {
  type Direction = Value
  val North, East, South, West, NorthEast, NorthWest, SouthEast, SouthWest = Value
}
abstract class AbstractGame(board: Board) {
  import Direction.{Direction, East, North, NorthEast, NorthWest, South, SouthEast, SouthWest, West}
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
class Board(val squareMatrix: Array[Array[Square]]) {
  import SquareType.SquareType

  val transposedMatrix = transpose(squareMatrix)
  val airChar = findTypeSym(SquareType.Air).getOrElse('.')
  val rockChar = findTypeSym(SquareType.Rock).getOrElse('x')

  def this(charData: Array[Array[Char]], air: Char, rock: Char) = this(charData.zipWithIndex.map(arrIndex => arrIndex._1.zipWithIndex.map(
    symIndex => if (symIndex._1 == air)  AirSquare(symIndex._2, arrIndex._2, symIndex._1) else
      if (symIndex._1 == rock) RockSquare(symIndex._2, arrIndex._2, symIndex._1) else
        OtherSquare(symIndex._2, arrIndex._2, symIndex._1)
  )))
  def this(rawData: List[String], air: Char, rock: Char) = this(rawData.map(_.toCharArray).toArray, air, rock)

  def apply(x: Int): Array[Square] = transposedMatrix(x)

  private def findTypeSym(squareType: SquareType): Option[Char] = squareMatrix.flatten.find(_.squareType == squareType).map(_.sym)
  private def transpose(matrix: Array[Array[Square]]): Array[Array[Square]] = matrix.head.indices.map(i => matrix.map(_(i))).toArray

  def show(squares: Set[Square]): Unit = {
    Console.err.println(s"${toString(squares)}")
  }

  private def toString(squares: Set[Square]): String = {
    squareMatrix.map(row => row.map(s => if (squares.contains(s)) '\u25AE' else s.sym).mkString("")).mkString("\n")
  }

  override def toString: String = {
    squareMatrix.map(row => row.map(square => if (square.free) square.sym else 'X').mkString("")).mkString("\n")
  }
}
class Game(board: Board) extends AbstractGame(board) with Neighbours {

}
trait Neighbours extends AbstractGame {
  import Direction.Direction
  implicit val defaultStep: Int = 1
  implicit val defaultDirection: Set[Direction] = cardinal
  implicit val defaultValid: Square => Boolean = _ => true

  private def collectSquares(x: Int, y: Int, direction: Direction, step: Int, valid: Square => Boolean) = {
    (for (i <- 1 to step) yield takeRawSquareTry(x, y, direction, i)).collect {
      case ts if ts.isSuccess => ts.get
    }
  }

  private def collectSquaresWithDirection(x: Int, y: Int, direction: Direction, step: Int, valid: Square => Boolean) = {
    (for (i <- 1 to step) yield takeRawSquareTry(x, y, direction, i)).collect {
      case ts if ts.isSuccess => (ts.get, direction)
    }
  }

  private def getSquaresWhile(x: Int, y: Int, directions: Set[Direction], step: Int, valid: Square => Boolean) = {
    directions.flatMap { direction =>
      collectSquares(x, y, direction, step, valid)
        .takeWhile(square => valid(square))
    }
  }

  private def getSquaresWithDirectionWhile(x: Int, y: Int, directions: Set[Direction], step: Int, valid: Square => Boolean) = {
    directions.flatMap { direction =>
      collectSquaresWithDirection(x, y, direction, step, valid)
        .takeWhile(square => valid(square._1))
    }
  }

  private def getSquares(x: Int, y: Int, directions: Set[Direction], step: Int, valid: Square => Boolean) = {
    directions.flatMap { direction =>
      collectSquares(x, y, direction, step, valid)
        .toSet.filter(valid(_))
    }
  }

  private def getSquaresWithDirection(x: Int, y: Int, directions: Set[Direction], step: Int, valid: Square => Boolean) = {
    directions.flatMap { direction =>
      (for (i <- 1 to step) yield takeRawSquareTry(x, y, direction, i)).collect {
        case ts if ts.isSuccess => (ts.get, direction)
      }.toSet.filter(s => valid(s._1))
    }
  }

  def neighboursWithDirection(x: Int, y: Int, directions: Set[Direction], step: Int, valid: Square => Boolean) = getSquaresWithDirection(x, y, directions, step, valid)
  def neighboursWithDirection(x: Int, y: Int, step: Int, valid: Square => Boolean) = getSquaresWithDirection(x, y, defaultDirection, step, valid)
  def neighboursWithDirection(x: Int, y: Int, directions: Set[Direction], valid: Square => Boolean) = getSquaresWithDirection(x, y, directions, defaultStep, valid)
  def neighboursWithDirection(x: Int, y: Int, directions: Set[Direction], step: Int) = getSquaresWithDirection(x, y, directions, step, defaultValid)
  def neighboursWithDirection(x: Int, y: Int, valid: Square => Boolean) = getSquaresWithDirection(x, y, defaultDirection, defaultStep, valid)
  def neighboursWithDirection(x: Int, y: Int, step: Int) = getSquaresWithDirection(x, y, defaultDirection, step, defaultValid)
  def neighboursWithDirection(x: Int, y: Int, directions: Set[Direction]) = getSquaresWithDirection(x, y, directions, defaultStep, defaultValid)
  def neighboursWithDirection(x: Int, y: Int) = getSquaresWithDirection(x, y, defaultDirection, defaultStep, defaultValid)

  def neighbours(x: Int, y: Int, directions: Set[Direction], step: Int, valid: Square => Boolean) = getSquares(x, y, directions, step, valid)
  def neighbours(x: Int, y: Int, step: Int, valid: Square => Boolean) = getSquares(x, y, defaultDirection, step, valid)
  def neighbours(x: Int, y: Int, directions: Set[Direction], valid: Square => Boolean) = getSquares(x, y, directions, defaultStep, valid)
  def neighbours(x: Int, y: Int, directions: Set[Direction], step: Int) = getSquares(x, y, directions, step, defaultValid)
  def neighbours(x: Int, y: Int, valid: Square => Boolean) = getSquares(x, y, defaultDirection, defaultStep, valid)
  def neighbours(x: Int, y: Int, step: Int) = getSquares(x, y, defaultDirection, step, defaultValid)
  def neighbours(x: Int, y: Int, directions: Set[Direction]) = getSquares(x, y, directions, defaultStep, defaultValid)
  def neighbours(x: Int, y: Int) = getSquares(x, y, defaultDirection, defaultStep, defaultValid)

  def neighboursWhile(x: Int, y: Int, directions: Set[Direction], step: Int, valid: Square => Boolean) = getSquaresWhile(x, y, directions, step, valid)
  def neighboursWhile(x: Int, y: Int, step: Int, valid: Square => Boolean) = getSquaresWhile(x, y, defaultDirection, step, valid)
  def neighboursWhile(x: Int, y: Int, directions: Set[Direction], valid: Square => Boolean) = getSquaresWhile(x, y, directions, defaultStep, valid)
  def neighboursWhile(x: Int, y: Int, directions: Set[Direction], step: Int) = getSquaresWhile(x, y, directions, step, defaultValid)
  def neighboursWhile(x: Int, y: Int, valid: Square => Boolean) = getSquaresWhile(x, y, defaultDirection, defaultStep, valid)
  def neighboursWhile(x: Int, y: Int, step: Int) = getSquaresWhile(x, y, defaultDirection, step, defaultValid)
  def neighboursWhile(x: Int, y: Int, directions: Set[Direction]) = getSquaresWhile(x, y, directions, defaultStep, defaultValid)
  def neighboursWhile(x: Int, y: Int) = getSquaresWhile(x, y, defaultDirection, defaultStep, defaultValid)

  def neighboursWithDirectionWhile(x: Int, y: Int, directions: Set[Direction], step: Int, valid: Square => Boolean) = getSquaresWithDirectionWhile(x, y, directions, step, valid)
  def neighboursWithDirectionWhile(x: Int, y: Int, step: Int, valid: Square => Boolean) = getSquaresWithDirectionWhile(x, y, defaultDirection, step, valid)
  def neighboursWithDirectionWhile(x: Int, y: Int, directions: Set[Direction], valid: Square => Boolean) = getSquaresWithDirectionWhile(x, y, directions, defaultStep, valid)
  def neighboursWithDirectionWhile(x: Int, y: Int, directions: Set[Direction], step: Int) = getSquaresWithDirectionWhile(x, y, directions, step, defaultValid)
  def neighboursWithDirectionWhile(x: Int, y: Int, valid: Square => Boolean) = getSquaresWithDirectionWhile(x, y, defaultDirection, defaultStep, valid)
  def neighboursWithDirectionWhile(x: Int, y: Int, step: Int) = getSquaresWithDirectionWhile(x, y, defaultDirection, step, defaultValid)
  def neighboursWithDirectionWhile(x: Int, y: Int, directions: Set[Direction]) = getSquaresWithDirectionWhile(x, y, directions, defaultStep, defaultValid)
  def neighboursWithDirectionWhile(x: Int, y: Int) = getSquaresWithDirectionWhile(x, y, defaultDirection, defaultStep, defaultValid)

}
object Square {
  import SquareType.{Air, Rock, SquareType}

  def of(x: Int, y: Int, sym: Char, squareType: SquareType): Square =
    squareType match {
      case Air => AirSquare(x, y, sym)
      case Rock => RockSquare(x, y, sym)
      case _ => OtherSquare(x, y, sym)
    }
}
sealed abstract class Square(val x: Int, val y: Int) {
  import SquareType.SquareType

  def air: Boolean
  def rock: Boolean
  def other: Boolean
  def sym: Char
  def squareType: SquareType
  var free: Boolean = true
  var opp: Boolean = true

}
case class AirSquare(override val x: Int, override val y: Int, override val sym: Char) extends Square(x, y) {
  import SquareType.Air
  override val air = true
  override val rock = false
  override val other = false
  override val squareType = Air
}
case class RockSquare(override val x: Int, override val y: Int, override val sym: Char) extends Square(x, y) {
  import SquareType.Rock
  override val air = false
  override val rock = true
  override val other = false
  override val squareType = Rock
}
case class OtherSquare(override val x: Int, override val y: Int, override val sym: Char) extends Square(x, y) {
  import SquareType.Other
  override val air = false
  override val rock = false
  override val other = true
  override val squareType = Other
}
object Util {

  def readAsList(filename: String): List[String] = {
    readAsTry(filename) match {
      case Success(lines) => lines
      case Failure(e) =>
        Console.err.println(s"${e.getMessage}")
        List.empty
    }
  }

  def readAsTry(filename: String): Try[List[String]] = Using(Source.fromFile(filename)) { _.getLines.toList }
  def linesToBoard(lines: List[String], air: Char = '.', rock: Char = 'x'): Board = new Board(lines, air, rock)
}


object Player extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
  val filename = "resources/voxcodei/foresee-the-future.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines
  def readInt = if (data.hasNext) data.next.toInt else { System.exit(0); -1 }
  def readLine = if (data.hasNext) data.next else { System.exit(0); "" }
//----------------------------------------------------------------------------------------------------------------------

  val Array(width, height) = (readLine split " ").filter(_ != "").map (_.toInt)
  Console.err.println(s"$width $height")
  var countdownMap = Map.empty[Square, Int]
  val lines = (for(i <- 0 until height) yield readLine).toList
  var board = Util.linesToBoard(lines, rock = '#')
  Console.err.println(s"${board.toString}")

  def findTarget(square: Square, game: Game) = game.neighboursWithDirectionWhile(square.x, square.y, game.cardinal, 3, !_.isInstanceOf[RockSquare]).map(_._1).filter(_.other)

  def update(board: Board, maxSquare: Square, maxTargets: Set[Square]) = {
    def createAirSquare(x: Int, y: Int) = {
      val square = Square.of(x, y, board.airChar, SquareType.Air)
      square.free = false
      square
    }
    val newMatrix = board.squareMatrix.clone
    maxTargets.foreach(target => newMatrix(target.y)(target.x) = createAirSquare(target.x, target.y))
    newMatrix(maxSquare.y)(maxSquare.x).free = false
    new Board(newMatrix)
  }
  def findSquare(board: Board) = {
    val game = new Game(board)
    val squareTypeArr = board.squareMatrix.flatten
    val freeSquares = squareTypeArr.filter(square => square.squareType == SquareType.Air && square.free).toSet
    val squareTargetMap = freeSquares.map(square => (square, findTarget(square, game))).toMap
    val squareAmountMap = freeSquares.map(square => (findTarget(square, game).size, square)).toMap
//    val squareAmountMap = squareTargetMap.map(entry => (entry._2.size, entry._1)).collect {
//
//    }
    val maxAmount = squareAmountMap.keysIterator.max
    val maxSquare = squareAmountMap(maxAmount)
    val maxTargets = squareTargetMap(maxSquare)
    (maxSquare, maxTargets)
  }

  def calculate(globalRounds: Int, globalBombs: Int) = {
    var rounds = globalRounds
    var bombs = globalBombs
    while (rounds > 0 && bombs > 0) {
      val (maxSquare, maxTargets) = findSquare(board)
      board = update(board, maxSquare, maxTargets)


    }
  }

  while(true) {
    countdownMap = countdownMap.map(kv => (kv._1, kv._2 - 1)).filterNot(_._2 == 0)
    val Array(rounds, bombs) = (readLine split " ").filter(_ != "").map (_.toInt)
    Console.err.println(s"$rounds $bombs")

    calculate(rounds, bombs)

    val (maxSquare, maxTargets) = findSquare(board)
    board = update(board, maxSquare, maxTargets)

    countdownMap = countdownMap + (maxSquare -> 3)
    println(s"${maxSquare.x} ${maxSquare.y}")
  }
}