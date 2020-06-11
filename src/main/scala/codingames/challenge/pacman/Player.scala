package codingames.challenge.pacman

import TargetType.TargetType
import scala.io.Source
import scala.util._
import scala.io.StdIn._

object Direction extends Enumeration {
  type Direction = Value
  val North, East, South, West, NorthEast, NorthWest, SouthEast, SouthWest = Value
}
object TargetType extends Enumeration {
  type TargetType = Value
  val MaxPrise, Clash, Enemy, RunAway, MinPrise, Unvisited = Value
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

  var num: Int = 0
  var prise: Int = 0
  var free: Boolean = true
  var opp: Boolean = true

  override def toString: String = {
    s"$x $y"
  }
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
  val size = toSet.size
  val width = transposedMatrix.length
  val height = squareMatrix.length

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


  def show(squares: Set[Square]) {
     Console.err.println(s"${toString(squares)}")
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
      case West => Try(board( { if (x == 0) board.width else x } - step)(y))
      case East => Try(board( (x + step) % board.width)(y))
      case NorthWest => Try(board(x - step)(y - step))
      case SouthWest => Try(board(x - step)(y + step))
      case NorthEast => Try(board(x + step)(y - step))
      case SouthEast => Try(board(x + step)(y + step))
    }

  }

  def inMoves(x: Int, y: Int, num: Int) = {
    board(x)(y).num = 0
    var stack = List(board(x)(y))
    var out = Set.empty[Square]
    val marked = Array.fill[Boolean](board.size)(false)

    while (stack.nonEmpty) {
      val e = stack.head
      marked(e.index) = true
      val neighbours = cardinal.map(takeRawSquareTry(e.x, e.y, _, 1)).collect {
        case res if res.isSuccess => res.get
      }.filterNot(s => s.rock || marked(s.index))
      neighbours.foreach(_.num = e.num + 1)
      out = out ++ neighbours
      stack = neighbours.filter(_.num < num).toList ::: stack.tail
    }
    out
  }

  def bfs(x: Int, y: Int) = {
    var i = 0
    val square = board(x)(y)
    var stack = List(square)
    val marked = new Array[Boolean](board.size)
    val edgeTo = Array.fill[Int](board.size)(Int.MaxValue)
    val distTo = Array.fill[Int](board.size)(Int.MaxValue)

    distTo(square.index) = 0
    edgeTo(square.index) = -1

    while (stack.nonEmpty) {
      val e = stack.head
      i = i + 1
      marked(e.index) = true
      val neighbours = cardinal.map(takeRawSquareTry(e.x, e.y, _, 1)).collect {
        case res if res.isSuccess => res.get
      }.filterNot(s => s.rock || marked(s.index))
      neighbours.foreach(square => {
        marked(square.index) = true
        edgeTo(square.index) = e.index
        distTo(square.index) = distTo(e.index) + 1
      })
      stack = stack.tail ::: neighbours.toList
    }
    (edgeTo, distTo)
  }

  def path(fromX: Int, fromY: Int, edgeTo: Array[Int]) = {
    val square = board(fromX)(fromY)
    var next = edgeTo(square.index)
    var out = List.empty[Int]

    while (next != -1) {
      out = next :: out
      next =  edgeTo(next)
    }
    out
  }
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
class Game(board: Board) extends AbstractGame(board: Board) with Neighbours
class Pac(val pacId: Int, val mine: Boolean, var x: Int, var y: Int, var typeId: String, var speedTurnsLeft: Int, var abilityCooldown: Int, var live: Boolean = true) {
  import TargetType.TargetType

  var needTarget = true
  var target: Square = _
  var clash = false
  var back: Square = _
  var targetType: TargetType = _
  var prevaction = ""
  var action = "MOVE"
  def setAction(str: String) = { prevaction = action; action = str; }
  def setTarget(t: Square, ttype: TargetType): Unit = {
    Console.err.println(s"$pacId setTarget=$t back=$back type=$ttype")
    target = t; needTarget = false; targetType = ttype;
  }
  def todo = if (action == "MOVE") {
    if (target == null) "null" else s"$target"
  } else if (action == "SWITCH") s"$typeId" else ""
  def command = s"${action} $pacId $todo <${if (target != null) target else "null"}:$targetType>"
  def reset = {
    live = false; clash = false; setAction("MOVE")
  }
  override def toString: String = s"Pac($x,$y)"
}












object Player extends App {
  val rand = new Random
  val winpacmap = Map("SCISSORS" -> "ROCK", "ROCK" -> "PAPER", "PAPER" -> "SCISSORS")
  val losepacmap = Map("PAPER" -> "ROCK", "SCISSORS" -> "PAPER", "ROCK" -> "SCISSORS")
  def dummy(squares: Set[Square], pac: Pac) = squares
  def directY(square: Square, pac: Pac) = Math.abs(pac.y - square.y)
  def directX(square: Square, pac: Pac) = Math.abs(pac.x - square.x)
  def onlinePrise(squares: Set[Square], pac: Pac) = visibleSquaresMap(board(pac.x)(pac.y)).intersect(squares)
  def goodPac(pac: Pac) = pac != null && pac.live && !pac.typeId.startsWith("D")
  def linesToBoard(lines: List[String]): Board = new Board(lines, ' ', '#')

//------------------------------------------FILE ENTRY------------------------------------------------------------------
//      val filename = "resources/pacman/pacman5.txt"
//      val bufferedSource = Source.fromFile(filename)
//      val data = bufferedSource.getLines
//      def readInt = if (data.hasNext) data.next.toInt else -1
//      def readLine = if (data.hasNext) data.next else "EOF"
//----------------------------------------------------------------------------------------------------------------------
  def calculateEuclidean(pacSquare: Square, s2: Square) = {
    val direct = Calc.euclidean((pacSquare.x, pacSquare.y), (s2.x, s2.y))
    if (pacSquare.x == 0) {
      Math.min(direct, Calc.euclidean((s2.x, s2.y), (board.width, pacSquare.y)))
    } else if (pacSquare.x == board.width - 1) {
      Math.min(direct, Calc.euclidean((s2.x, s2.y), (-1, pacSquare.y)))
    } else direct
  }

  def calculateEuclidean(pac: Pac, square: Square) = {
    val direct = Calc.euclidean((square.x, square.y), (pac.x, pac.y))
    if (pac.x == 0) {
      Math.min(direct, Calc.euclidean((square.x, square.y), (board.width, pac.y)))
    } else if (pac.x == board.width - 1) {
      Math.min(direct, Calc.euclidean((square.x, square.y), (-1, pac.y)))
    } else direct
  }
  def resolveMyClash(pacs: Set[Pac]) {
    val clashTarget = pacs.filter(pac => pac.clash && pac.needTarget)
    Console.err.println(s"clash $clashTarget")
    if (clashTarget.nonEmpty) {
      val minPac = clashTarget.minBy(_.pacId)
      Console.err.println(s"minPac=${minPac.pacId}")
      val squares = game.inMoves(minPac.x, minPac.y, 2)
      Console.err.println(s"squares in two=$squares")
      val pacsq = squares.find(square => pacs.map(pac => board(pac.x)(pac.y)).contains(square))
      Console.err.println(s"pac in clash=$pacsq")
      val clashTargetSquare = pacsq match {
        case Some(sq) => {
          Console.err.println(s"pac=$sq")
          val res = squares.maxBy(square => calculateEuclidean(sq, square))
          Console.err.println(s"fraest square=$res")
          res
        }
        case None => {
          Console.err.println(s"Set back: ${minPac.back}")
          minPac.back
        }
      }

      minPac.setTarget(clashTargetSquare, TargetType.Clash)
    }
  }
  def cannotEatOpp(pac: Pac, typeOppMap: Map[String, Set[Square]], canEatMe: Option[(String, Set[Square])]) {
    if (pac.abilityCooldown == 0) {
      val firstOpp = typeOppMap.head
      val oppType = firstOpp._1
      val oppSquare = firstOpp._2.head
//      Console.err.println(s"(${pac.x},${pac.y}) cannotEatOpp and switch: oppSquare=$oppSquare type=$oppType target=${pac.target}")
      pac.setAction("SWITCH")
      pac.typeId = winpacmap(oppType)
      pac.setTarget(oppSquare, TargetType.Enemy)
    } else {
      canEatMe match {
        case Some((_, oppset)) =>  {
          val oppSq = oppset.head
//          Console.err.println(s"(${pac.x},${pac.y}) cannotEatOpp and go: opp=$oppSq target=${pac.target}")
          val squares = game.inMoves(pac.x, pac.y, 1) + board(pac.x)(pac.y)
          val newSq = squares.maxBy(sq => Calc.euclidean(sq, oppSq))
//          val newSq = squares.maxBy(sq => distanceMap((sq, oppSq)))
          pac.setTarget(newSq, TargetType.RunAway)
        }
        case None =>
      }
    }
  }
  def calculateEnemy(pac: Pac, oppHere: Set[Square], spac: Map[Square, Pac]) {
    val typeOppMap = oppHere.groupBy(opp => spac(opp).typeId)

    val canEatOpp = typeOppMap.find(oppType => {
//      Console.err.println(s"\tfind my ${pac.typeId} == opp ${oppType._1} need ${winpacmap(oppType._1)}")
      winpacmap(oppType._1) == pac.typeId
    })
    val canEatMe = typeOppMap.find(oppType => {
//      Console.err.println(s"\tfind my ${pac.typeId} == opp ${oppType._1} need ${losepacmap(oppType._1)}")
      losepacmap(oppType._1) == pac.typeId
    })

    canEatOpp match {
      case Some((_, oppset)) =>  {
//        Console.err.println(s"(${pac.x},${pac.y}) canEatOpp: ${oppset.head} target=${pac.target}")
        pac.setTarget(oppset.head, TargetType.Enemy)
      }
      case None => cannotEatOpp(pac, typeOppMap, canEatMe)
    }
  }
  def resolveEnemies(pacs: Set[Pac], oppacset: Set[Pac], game: Game) {
    pacs.filter(_.needTarget).foreach(pac => {
      val squares = game.inMoves(pac.x, pac.y, 4)
      val squarePacMap = oppacset.map(p => (board(p.x)(p.y), p)).toMap
      val oppHere = squares.intersect(squarePacMap.keySet)
      if (oppHere.nonEmpty) {
        calculateEnemy(pac, oppHere, squarePacMap)
      }
    })
  }
  def findSquare(pacs: Set[Pac], sureSquares: Set[Square], unvisitedSquares: Set[Square]) {
    setBigPriseTarget(pacs, squares.filter(_.prise == 10), dummy)                                  // prise = 10
    setSingleTarget(pacs, sureSquares.filter(_.prise == 1), onlinePrise, TargetType.MinPrise)                      // visible prise on the same line
    setSingleTarget(pacs, unvisitedSquares, dummy, TargetType.Unvisited)                                            // unvisited squares
  }
  def setBigPriseTarget(pacs: Set[Pac], squares: Set[Square], f: (Set[Square], Pac) => Set[Square]) {
//    val squaresPacs = pacs.map(pac => (board(pac.x)(pac.y), pac))
    val cartesian = pacs.withFilter(_.needTarget).flatMap(pac => squares.map(priseSquare => (priseSquare, board(pac.x)(pac.y), pac,
      calculateEuclidean(pac, priseSquare)
    )))
//      distanceMap((priseSquare, squarePac._1)))))
    var sortedData = cartesian.toList.sortBy(_._4)
    while (sortedData.nonEmpty) {
      val pair = sortedData.head
      pair._3.setTarget(pair._1, TargetType.MaxPrise)
      sortedData = sortedData.filterNot(data => data._1 == pair._1 || data._3 == pair._3)
//      Console.err.println(s"$sortedData")
    }
  }
  def setSingleTarget(pacs: Set[Pac], squares: Set[Square], f: (Set[Square], Pac) => Set[Square], ttype: TargetType): Set[Square] = {
    var targetSquare = squares
    pacs.filter(pac => pac.needTarget
//      && (pac.targetType != TargetType.Unvisited || board(pac.x)(pac.y) == pac.target)
    ).foreach(pac => {
      val filteredSquare = f(targetSquare, pac)
      if (filteredSquare.nonEmpty) {
        val square = if (pac.speedTurnsLeft > 0) {
          val sortedSquares = filteredSquare.toList.sortWith((s1,s2) => {
            calculateEuclidean(pac, s1) < calculateEuclidean(pac, s2)
          })
          if (sortedSquares.length > 1) {
            sortedSquares.tail.head
          } else sortedSquares.head
        } else {
          filteredSquare.minBy(square =>
//          distanceMap((square, board(pac.x)(pac.y))))
          calculateEuclidean(pac, square))
        }
        targetSquare = targetSquare - square
        pac.setTarget(square, ttype)
      }
    })
    targetSquare
  }
  def resolveMySpeed(mypacset: Set[Pac], myScore: Int, oppScore: Int) {
    if (myScore <= oppScore) {
      mypacset.foreach(mypac => if (mypac.abilityCooldown == 0) mypac.setAction("SPEED"))
    }
  }
  def calculateTargets(mypacset: Set[Pac], oppacset: Set[Pac], sureSquares: Set[Square], unvisitedSquares: Set[Square], game: Game,
                       myScore: Int, oppScore: Int) {
    resolveEnemies(mypacset, oppacset, game)
    resolveMyClash(mypacset)
    resolveMySpeed(mypacset, myScore, oppScore)
    findSquare(mypacset, sureSquares, unvisitedSquares)
  }
  def calculateUnvisitedSquares(unvisitedSquares: Set[Square], mypacset: Set[Pac], surePelet: Set[Square], game: Game) = {
    val allVisible = mypacset.flatMap(pac => visibleSquaresMap(board(pac.x)(pac.y)))
    val allEmpty = allVisible.filterNot(surePelet)
    unvisitedSquares.filterNot(allEmpty)
  }

  val Array(width, height) = (readLine split " ").map(_.toInt)
//  Console.err.println(s"${width} $height")
  val lines = (for (i <- 0 until height) yield readLine).toList
  val board = linesToBoard(lines)
  val game = new Game(board)
//  Console.err.println(s"${board.toString}")

  val squares = board.toSet.filter(_.air)
  var visitedSquares = Set.empty[Square]
  var unvisitedSquares = squares
  var mypac = Array.fill[Pac](5)(null)
  var oppac = Array.fill[Pac](5)(null)
  val visibleSquaresMap = squares.map(square => (square, game.neighboursWhile(square.x, square.y, game.cardinal, width, _.air))).toMap

//  var distanceMap = Map.empty[(Square, Square), Int]
//  val bfsMap = squares.map(square => {
//    val bfs = game.bfs(square.x, square.y)
//    (square, bfs)
//  }).toMap

//  for (square1 <- squares; square2 <- squares; if !distanceMap.contains((square1, square2))) {
//    val dist = bfsMap(square1)._2(square2.index)
//    distanceMap = distanceMap + ((square1, square2) -> dist)
//    distanceMap = distanceMap + ((square2, square1) -> dist)
//  }



  while (true) {
    val Array(myScore, oppScore) = (readLine split " ").map(_.toInt)
//     Console.err.println(s"$myScore $opponentScore")
    val visiblePacCount = readLine.toInt
//     Console.err.println(s"$visiblePacCount")

    mypac.filterNot(_ == null).foreach(_.reset)
    oppac.filterNot(_ == null).foreach(_.reset)
/*
    Console.err.println(s"BEFORE!!!")
    mypac.filterNot(_ == null).foreach(pac => {
      Console.err.println(s"id-${pac.pacId} - <${pac.action}> ${pac.command} ${pac.target}")
    })*/

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
        val pac = targetPac(pacId)
        val stop = pac.x == x && pac.y == y
        pac.needTarget = true
        pac.clash = {
          Console.err.print(s"${pac.pacId} $stop ${pac.action} ${pac.prevaction}")
          stop && pac.prevaction == "MOVE"
        }
        Console.err.println(s" -> ${pac.clash}")
        if (!stop) pac.back = board(pac.x)(pac.y)
        pac.x = x
        pac.y = y
        pac.typeId = typeId
        pac.speedTurnsLeft = speedTurnsLeft
        pac.abilityCooldown = abilityCooldown
        pac.live = true

      }
//       Console.err.println(s"${_pacId} ${_mine} ${_x} ${_y} $typeId ${_speedTurnsLeft} ${_abilityCooldown}")
    }

    val visiblePelletCount = readLine.toInt
//     Console.err.println(s"$visiblePelletCount")
//    squares.foreach(_.prise = 0)
//     Console.err.println(s"<pellet...>")

    var surePelet = Set.empty[Square]

    squares.foreach(square => if (square.prise == 10) square.prise = 0)

    for (i <- 0 until visiblePelletCount) {
      val Array(x, y, value) = (readLine split " ").map(_.toInt)
      board(x)(y).prise = value
      surePelet = surePelet + board(x)(y)
//       Console.err.println(s"pel: ($x,$y) $value")
    }

    val mypacset = mypac.filter(goodPac).toSet
    val opppacset = oppac.filter(goodPac).toSet


    unvisitedSquares = calculateUnvisitedSquares(unvisitedSquares, mypacset, surePelet, game)
    calculateTargets(mypacset, opppacset, surePelet, unvisitedSquares, game, myScore, oppScore)
//    speedUp(mypacset)
//    findOpp(mypacset, opppacset, game)
//    setTarget(mypacset, surePelet.toSet)
//    setTarget(mypacset, unvisitedSquares)
/*
    Console.err.println(s"AFTER!!!")
    mypacset.foreach(pac => {
      Console.err.println(s"id-${pac.pacId} - ${pac.command} ${pac.target}")
    })*/

    val commands = mypacset.map(_.command).mkString("|")

    println(commands)
  }
}