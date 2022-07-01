package codingames.hard.voxcodei

import scala.annotation.tailrec
import scala.io.Source
import scala.math._
import scala.util._

object Calc {
  def euclidean(a: (Int, Int), b: (Int, Int)): Double = sqrt(pow(b._1 - a._1, 2) + pow(b._2 - a._2, 2))
  def euclidean(a: Square, b: Square): Double = sqrt(pow(b.x - a.x, 2) + pow(b.y - a.y, 2))
}
object SquareType extends Enumeration {
  type SquareType = Value
  val Air, Rock, Target, Bomb = Value
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
        TargetSquare(symIndex._2, arrIndex._2, symIndex._1)
  )))
  def this(rawData: List[String], air: Char, rock: Char) = this(rawData.map(_.toCharArray).toArray, air, rock)

  def apply(x: Int): Array[Square] = transposedMatrix(x)

  def allSquares = squareMatrix.flatten
  private def findTypeSym(squareType: SquareType): Option[Char] = allSquares.find(_.squareType == squareType).map(_.sym)
  private def transpose(matrix: Array[Array[Square]]): Array[Array[Square]] = matrix.head.indices.map(i => matrix.map(_(i))).toArray

  def show(squares: Set[Square]): Unit = {
    Console.err.println(s"${toString(squares)}")
  }

  private def toString(squares: Set[Square]): String = {
    squareMatrix.map(row => row.map(s => if (squares.contains(s)) '\u25AE' else if (s.underAttack) 'A' else s.sym).mkString("")).mkString("\n")
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
  import SquareType.{Air, Rock, Target, Bomb, SquareType}

  def of(x: Int, y: Int, sym: Char, squareType: SquareType): Square =
    squareType match {
      case Air => AirSquare(x, y, sym)
      case Rock => RockSquare(x, y, sym)
      case Target => TargetSquare(x, y, sym)
      case Bomb => BombSquare(x, y, sym)
      case _ => throw new IllegalArgumentException("What the hell are you trying to create?!")
    }
}
sealed abstract class Square(val x: Int, val y: Int) {
  import SquareType.SquareType

  def air: Boolean
  def rock: Boolean
  def target: Boolean
  def bomb: Boolean
  def sym: Char
  def countdown: Unit
  def squareType: SquareType
  var targets: Set[Square] = Set.empty
  var free: Boolean = true
  var underAttack: Boolean = false
  var explosionTime: Int = -1
  val action = (x, y)
}
case class AirSquare(override val x: Int, override val y: Int, override val sym: Char) extends Square(x, y) {
  import SquareType.Air
  override val air = true
  override val rock = false
  override val target = false
  override val bomb = false
  override val countdown: Unit = {}
  override val squareType = Air
}
case class RockSquare(override val x: Int, override val y: Int, override val sym: Char) extends Square(x, y) {
  import SquareType.Rock
  override val air = false
  override val rock = true
  override val target = false
  override val bomb = false
  override val countdown: Unit = {}
  override val squareType = Rock
}
case class TargetSquare(override val x: Int, override val y: Int, override val sym: Char) extends Square(x, y) {
  import SquareType.Target
  override val air = false
  override val rock = false
  override val target = true
  override val bomb = false
  override val countdown: Unit = {}
  override val squareType = Target
}
case class BombSquare(override val x: Int, override val y: Int, override val sym: Char) extends Square(x, y) {
  import SquareType.Bomb
  explosionTime = 3
  override val air = false
  override val rock = false
  override val target = false
  override val bomb = true
  override val countdown: Unit = { explosionTime = explosionTime - 1 }
  override val squareType = Bomb
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
  val filename = "resources/voxcodei/foresee-the-future-better.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines
  def readInt = if (data.hasNext) data.next.toInt else { System.exit(0); -1 }
  def readLine = if (data.hasNext) data.next else { System.exit(0); "" }
//----------------------------------------------------------------------------------------------------------------------

  type Action = Either[(Int, Int), String]
  val Array(width, height) = (readLine split " ").filter(_ != "").map (_.toInt)
  Console.err.println(s"$width $height")
  var countdownMap = Map.empty[Square, Int]
  val lines = (for(i <- 0 until height) yield readLine).toList
  var board = Util.linesToBoard(lines, rock = '#')
  Console.err.println(s"${board.toString}")

  def findTarget(square: Square, game: Game) = game.neighboursWithDirectionWhile(square.x, square.y, game.cardinal, 3, !_.rock)
    .map(_._1)
    .filter(_.target)

  def update2(board: Board, maxSquare: Square, maxTargets: Set[Square]) = {
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

  def calculateSquares(board: Board) = {
    val game = new Game(board)
    val freeSquares = board.allSquares.filter(square => square.squareType == SquareType.Air && square.free && !square.underAttack).toList
    freeSquares.map(square => (square, findTarget(square, game)))
  }

  def calculateMaps(board: Board) = {
    val squareSet = calculateSquares(board)
    val squareTargetMap = squareSet.toMap
    val squareAmountMap = squareSet.map(squares => (squares._2.size, squares._1)).toMap
    (squareTargetMap, squareAmountMap)
  }

  def findMaxSquare(board: Board) = {
    val (squareTargetMap, squareAmountMap) = calculateMaps(board)
    val maxAmount = squareAmountMap.keysIterator.max
    val maxSquare = squareAmountMap(maxAmount)
    val maxTargets = squareTargetMap(maxSquare)
    (maxSquare, maxTargets)
  }

//  def update(board: Board, maxSquare: Square, maxTargets: Set[Square]) = {
  def update(board: Board, action: Action, maxTargets: Set[Square]) = {
    val newMatrix = board.squareMatrix.map(row => row.map(identity))
    action match {
      case Left((x, y)) =>
        newMatrix(y)(x) = Square.of(x, y, '*', SquareType.Bomb)
        newMatrix(y)(x).targets = maxTargets
        maxTargets.foreach(square => newMatrix(square.y)(square.x).underAttack = true)
        board.allSquares.foreach(square => newMatrix(square.y)(square.x).countdown)
      case Right(_) =>
        board.allSquares.foreach(square => newMatrix(square.y)(square.x).countdown)
    }
    for (row <- newMatrix.indices; column <- newMatrix(row).indices) {
      if (newMatrix(row)(column).explosionTime == 0) {
        newMatrix(row)(column).targets.foreach(target => {
          newMatrix(target.y)(target.x) = Square.of(column, row, board.airChar, SquareType.Air)
        })
        newMatrix(row)(column) = Square.of(column, row, board.airChar, SquareType.Air)
      }
    }
    new Board(newMatrix)
  }

  @tailrec
  def calculate(board: Board, globalRounds: Int, globalBombs: Int, actions: List[Action]): List[Action] = {
    if (board.allSquares.forall(square => !square.target || square.underAttack)) actions else
    if (globalBombs == 0) actions else
    if (globalRounds == 0) actions
    else {
      val (maxSquare, maxTargets) = findMaxSquare(board)
      val updatedActions = Left(maxSquare.action) :: actions
      val updatedBombs = globalBombs - 1
      val updatedRounds = globalRounds - 1
      val updatedBoard = update(board, Left(maxSquare.action), maxTargets)
      calculate(updatedBoard, updatedRounds, updatedBombs, updatedActions)
    }
  }

  while(true) {
    countdownMap = countdownMap.map(kv => (kv._1, kv._2 - 1)).filterNot(_._2 == 0)
    val Array(rounds, bombs) = (readLine split " ").filter(_ != "").map (_.toInt)
    Console.err.println(s"$rounds $bombs")

//    val (maxSquare, maxTargets) = findMaxSquare(board)
//    board = update(board, maxSquare, maxTargets)

//    val squares = calculateSquares(board).sortBy(_._2.size)
    calculate(board, rounds, bombs, List.empty)

//    countdownMap = countdownMap + (maxSquare -> 3)
//    println(s"${maxSquare.x} ${maxSquare.y}")
    println("WAIT")
  }
}