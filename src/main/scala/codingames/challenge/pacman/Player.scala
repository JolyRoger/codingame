//package codingames.challenge.pacman

import scala.collection.mutable
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

  var num: Int = 0
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
  val size = toSet.size

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
      case West => Try(board(x + step)(y))
      case East => Try(board(x - step)(y))
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
}
class Game(board: Board) extends AbstractGame(board: Board)
class Pac(val pacId: Int, val mine: Boolean, var x: Int, var y: Int, var typeId: String, var speedTurnsLeft: Int, var abilityCooldown: Int, var live: Boolean = true) {
  var needTarget = true
  var target: Square = _
  var clash = false
  var targetType = 0      // 0 - max prise, shortest distance,
                          // 1 - max prise, longest distance,
                          // 2 - min prise, shortest distance
                          // 3 - min prose, longest distance

  var action = "MOVE"
  def todo = if (action == "MOVE") {
    if (target == null) "null" else s"${target.x} ${target.y}"
  } else
    if (action == "SWITCH") s"$typeId" else ""
  def command = s"${action} $pacId $todo <${target.x},${target.y}>"
  def reset = {
    live = false; clash = false; action = "MOVE"
  }
}














object Player extends App {
  val rand = new Random
  val winpacmap = Map("SCISSORS" -> "ROCK", "ROCK" -> "PAPER", "PAPER" -> "SCISSORS")
  def linesToBoard(lines: List[String]): Board = new Board(lines, ' ', '#')
  def goodPac(pac: Pac) = pac != null && pac.live
//  def needTarget(pac: Pac) = pac.target == null ||
//    (pac.target.x == pac.x && pac.target.y == pac.y) ||
//    pac.target.prise == 0 ||
//    pac.clash
  def setTarget(pacs: Set[Pac], squares: Set[Square]) {
    var priseMap = squares.groupBy(_.prise) - 0

    val targetPacs = pacs.filter(_.needTarget)
    val (clashTarget, usualTarget) = targetPacs.partition(_.clash)
    usualTarget.foreach(_.targetType = 0)

    if (clashTarget.nonEmpty) {
      val minPac = clashTarget.minBy(_.targetType)
//      Console.err.println(s"minPac: ${minPac.pacId} ${minPac.targetType} ${minPac.target}")
      minPac.targetType = (minPac.targetType + 1) % 4
    }

    for (pac <- targetPacs) {
      if (priseMap.nonEmpty) {
        val result = setTarget(pac, priseMap)
        pac.target = result._1
        priseMap = result._2
      }
    }
  }
  def setTarget(pac: Pac, priseMap: Map[Int, Set[Square]]) = {
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
    (square, if (targetSet.isEmpty) priseMap - maxPrise else priseMap + (maxPrise -> targetSet))
  }
  def findOpp(mypac: Set[Pac], oppac: Set[Pac], game: Game) {
    mypac.foreach(pac => {
      val squares = game.inMoves(pac.x, pac.y, 4)
      val spac = oppac.map(p => (board(p.x)(p.y), p)).toMap
      val oppHere = squares.intersect(spac.keySet)
//      Console.err.println(s"(${pac.pacId})oppHere: $oppHere")
      if (oppHere.nonEmpty) {
        val typeOppMap = oppHere.groupBy(opp => spac(opp).typeId)
        val canEatOpp = typeOppMap.find(oppType => {
          Console.err.println(s"\tfind my ${pac.typeId} == opp ${oppType._1} need ${winpacmap(oppType._1)}")
          winpacmap(oppType._1) == pac.typeId
        })

        val newTarget = canEatOpp match {
          case Some((_, oppset)) =>  {
            Console.err.println(s"(${pac.x},${pac.y}) canEatOpp: ${oppset.head} target=${pac.target}")
            oppset.head
          }
          case None => if (pac.abilityCooldown == 0) {
            val firstOpp = typeOppMap.head
            val oppType = firstOpp._1
            val oppSquare = firstOpp._2.head
            Console.err.println(s"(${pac.x},${pac.y}) cannotEatOpp and switch: oppSquare=$oppSquare type=$oppType target=${pac.target}")
            pac.action = "SWITCH"
            pac.typeId = winpacmap(oppType)
            oppSquare
          } else {
            val oppSq = typeOppMap.head._2.head
            Console.err.println(s"(${pac.x},${pac.y}) cannotEatOpp and go: opp=$oppSq target=${pac.target}")
            val squares = game.inMoves(pac.x, pac.y, 1) + board(pac.x)(pac.y)
            val newSq =  squares.maxBy(sq => Calc.euclidean(sq, oppSq))
            Console.err.println(s"new target: $newSq")
            newSq
          }
        }
        pac.target = newTarget
      }
    })
  }

  //------------------------------------------FILE ENTRY------------------------------------------------------------------
//        val filename = "resources/pacman/pacman3.txt"
//        val bufferedSource = Source.fromFile(filename)
//        val data = bufferedSource.getLines
//        def readInt = if (data.hasNext) data.next.toInt else -1
//        def readLine = if (data.hasNext) data.next else "EOF"
  //----------------------------------------------------------------------------------------------------------------------


  val Array(width, height) = (readLine split " ").map(_.toInt)
   Console.err.println(s"${width} $height")
  val lines = (for (i <- 0 until height) yield readLine).toList
  val board = linesToBoard(lines)
  val game = new Game(board)
   Console.err.println(s"${board.toString}")

  val squares = board.toSet.filter(_.air)
  var visitedSquares = Set.empty[Square]
  var unvisitedSquares = squares
  var mypac = Array.fill[Pac](5)(null)
  var oppac = Array.fill[Pac](5)(null)


  def speedUp(mypacset: Set[Pac]) {
    mypacset.foreach(mypac => if (mypac.abilityCooldown == 0) mypac.action = "SPEED")
  }

  while (true) {
    val Array(myScore, opponentScore) = (readLine split " ").map(_.toInt)
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
        pac.needTarget = x == pac.target.x && y == pac.target.y
        pac.clash = pac.x == x && pac.y == y
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

    var surePelet = List.empty[Square]

    for (i <- 0 until visiblePelletCount) {
      val Array(x, y, value) = (readLine split " ").map(_.toInt)
      board(x)(y).prise = value
      surePelet = board(x)(y) :: surePelet
//       Console.err.println(s"$x $y $value")
    }

//    val res = game.inMoves(3, 1, 4)
//    board.show(res)

    val mypacset = mypac.filter(goodPac).toSet
    val opppacset = oppac.filter(goodPac).toSet


    speedUp(mypacset)
    findOpp(mypacset, opppacset, game)
    setTarget(mypacset, surePelet.toSet)
    setTarget(mypacset, unvisitedSquares)
/*
    Console.err.println(s"AFTER!!!")
    mypacset.foreach(pac => {
      Console.err.println(s"id-${pac.pacId} - ${pac.command} ${pac.target}")
    })*/

    val commands = mypac.collect {
      case pac if goodPac(pac) => pac.command
    }.mkString("|")

    println(commands)
  }
}