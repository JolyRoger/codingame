package codingames.hard.voxcodei

import scala.io.Source
import scala.math._
import scala.util._
import scala.io.StdIn._

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

  def clear() = !allSquares.exists(_.target)
  def allSquares = squareMatrix.flatten
  def bombSquares = allSquares.filter(_.bomb)
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

  def of(x: Int, y: Int, sym: Char, squareType: SquareType): Square = {
    squareType match {
      case Air => AirSquare(x, y, sym)
      case Rock => RockSquare(x, y, sym)
      case Target => TargetSquare(x, y, sym)
      case Bomb => BombSquare(x, y, sym)
      case _ => throw new IllegalArgumentException("What the hell are you trying to create?!")
    }
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
  def print = println(s"$x $y")
  override def toString: String = s"[$x,$y]:$explosionTime"
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
  explosionTime = 4
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
//  val filename = "resources/voxcodei/foresee-the-future-better.txt"
//  val filename = "resources/voxcodei/foresee-the-future.txt"
  val filename = "resources/voxcodei/not-so-fast.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines
  def readInt = if (data.hasNext) data.next.toInt else { System.exit(0); -1 }
  def readLine = if (data.hasNext) data.next else { System.exit(0); "" }
//----------------------------------------------------------------------------------------------------------------------

  type SquareData = (Square, Set[Square])
  type Stack = List[SquareData]

  case class Action(action: Either[String, Square], marked: Boolean) {
    override def toString: String = action.toOption.map(_.toString).getOrElse("WAIT")
  }
  case class Transition(prevState: State, action: Action) {
    override def toString: String = action.toString
  }
  case class State(board: Board, rounds: Int, bombs: Int, transition: Transition, level: Int) {
    override def toString: String = transition + s" round=$rounds bombs=$bombs level=$level"
  }
  val Array(width, height) = (readLine split " ").filter(_ != "").map (_.toInt)
  Console.err.println(s"$width $height")
  var countdownMap = Map.empty[Square, Int]
  val lines = (for(i <- 0 until height) yield readLine).toList
  var board = Util.linesToBoard(lines, rock = '#')
  Console.err.println(s"${board.toString}")

  def findTarget(square: Square, game: Game) = game.neighboursWithDirectionWhile(square.x, square.y, game.cardinal, 3, !_.rock)
    .map(_._1)
    .filter(square => square.target && !square.underAttack)

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

  def calculateSquares(board: Board): List[Square] = {
    val game = new Game(board)
    val freeSquares = board.allSquares.filter(square => square.squareType == SquareType.Air && square.free).toList
    freeSquares.foreach(square => square.targets = findTarget(square, game))
    freeSquares
  }

  def calculateMaps(board: Board) = {
    val squareSet = calculateSquares(board)
    val squareAmountMap = squareSet.map(squares => (squares.targets.size, squares)).toMap
    squareAmountMap
  }

  def findMaxSquare(board: Board) = {
    val squareAmountMap = calculateMaps(board)
    squareAmountMap(squareAmountMap.keysIterator.max)
  }

  def update(board: Board, action: Action) = {
    val newMatrix = board.squareMatrix.map(row => row.map(identity))
    action.action match {
      case Right(square) =>
        val (x, y) = square.action
        val maxTargets = square.targets
        newMatrix(y)(x) = Square.of(x, y, '*', SquareType.Bomb)
        newMatrix(y)(x).targets = maxTargets
        maxTargets.foreach(square => {
          val newTarget = Square.of(square.x, square.y, '+', SquareType.Target)
          newTarget.underAttack = true
          newMatrix(square.y)(square.x) = newTarget
        })
        board.bombSquares.foreach(square => {
          val newBomb = Square.of(square.x, square.y, '*', SquareType.Bomb)
          newBomb.explosionTime = newMatrix(square.y)(square.x).explosionTime - 1
          newBomb.targets = newMatrix(square.y)(square.x).targets
          newMatrix(square.y)(square.x) = newBomb
        })
      case Left(_) =>
        board.bombSquares.foreach(square => {
          val newBomb = Square.of(square.x, square.y, '*', SquareType.Bomb)
          newBomb.explosionTime = newMatrix(square.y)(square.x).explosionTime - 1
          newBomb.targets = newMatrix(square.y)(square.x).targets
          newMatrix(square.y)(square.x) = newBomb
        })
    }
    for (row <- newMatrix.indices; column <- newMatrix(row).indices) {
      if (newMatrix(row)(column).explosionTime == 0) {
        newMatrix(row)(column).targets.foreach(target => {
          newMatrix(target.y)(target.x) = Square.of(target.x, target.y, board.airChar, SquareType.Air)
        })
        newMatrix(row)(column) = Square.of(column, row, board.airChar, SquareType.Air)
      }
    }
    new Board(newMatrix)
  }

  def calculateActions(state: State) = {
    if (state.rounds > 0) {
      (if (state.bombs > 0) {
        calculateSquares(state.board).filterNot(sq => sq.targets.isEmpty).sortBy(sqq => sqq.targets.size).reverse.map(sqqq => Right(sqqq)).take(5)
      }
      else List.empty) :+ Left("WAIT")
    } else {
      List.empty
    }
  }
  def transition(state: State): List[State] = {
    val actions = calculateActions(state).map(Action(_, false))   // FIXME: убрать на хрен этот параметр
    val newBoardAction = actions.map(action => (update(state.board, action), action))
    val newStates = newBoardAction.map(boardAction => State(boardAction._1,
      state.rounds - 1,
      if (boardAction._2.action.isRight) state.bombs - 1 else state.bombs,
      Transition(state, boardAction._2), state.level + 1))
    newStates
  }

//  def transition2(state: State): List[State] = {
//    val newBoards = state.actions.map(action => (update(state.board, action), action))
//    val newDoings = newBoards.map(boardAction => (boardAction._1, calculateActions(state/*, boardAction._1*/), boardAction._2))
//    val newActions = newDoings.map(data => (data._1, data._2.map(aa => Action(aa, marked = false)), data._3))
//    val newStates = newActions.map(data => State(data._1, state.rounds - 1, if (data._3.action.isRight) state.bombs - 1 else state.bombs, data._2, Transition(state, data._3), state.level + 1))
//    newStates
//  }

  def calculate(board: Board, globalRounds: Int, globalBombs: Int): List[Action] = {
    var stack = List.empty[State]
    val initialState = State(board, globalRounds, globalBombs, null, 0)
    stack = initialState :: stack
    var currentState = stack.head

    while (stack.headOption.exists(_.board.allSquares.exists(_.target))) {
      currentState = stack.head
      stack = stack.tail

//      Console.err.println(s"\tcurrent: ${currentState.toString}")
//      stack.foreach(state => Console.err.println(s"${state.toString}"))

      val nextStates = transition(currentState)
      stack = nextStates ::: stack
//      Console.err.println
    }

    Console.err.println("List output:")
    var actionList: List[Action] = List.empty

    while (currentState.transition != null) {
      actionList = currentState.transition.action :: actionList
      currentState = currentState.transition.prevState
    }
    actionList
  }

  var needToCalculate = true
  var actions = List.empty[Action]

  do {
    val Array(rounds, bombs) = (readLine split " ").filter(_ != "").map (_.toInt)
    if (needToCalculate) {
      actions = calculate(board, rounds, bombs)
      needToCalculate = false
    }
    if (actions.nonEmpty) {
      val action = actions.head
      actions = actions.tail
      action.action match {
        case Right(square) => square.print
        case Left(wait) => println(wait)
      }
    } else {
      println("WAIT")
    }
  } while(true);
}