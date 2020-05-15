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
  var back: Square = _
  var targetType = 0      // 0 - max prise, shortest distance,
                          // 1 - max prise, longest distance,
                          // 2 - min prise, shortest distance
                          // 3 - min prose, longest distance
  var prevaction = ""
  var action = "MOVE"
  def setAction(str: String) = { prevaction = action; action = str; }
  def setTarget(t: Square): Unit = { target = t; needTarget = false; }
  def todo = if (action == "MOVE") {
    if (target == null) "null" else s"${target.x} ${target.y}"
  } else
    if (action == "SWITCH") s"$typeId" else ""
  def command = s"${action} $pacId $todo <${target.x},${target.y}>"
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
  def onlinePrise(squares: Set[Square], pac: Pac) = squares.filter(square => pac.x == square.x || pac.y == square.y)
  def goodPac(pac: Pac) = pac != null && pac.live
  def linesToBoard(lines: List[String]): Board = new Board(lines, ' ', '#')

  //------------------------------------------FILE ENTRY------------------------------------------------------------------
//        val filename = "resources/pacman/pacman4.txt"
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
    mypacset.foreach(mypac => if (mypac.abilityCooldown == 0) mypac.setAction("SPEED"))
  }












  def resolveMyClash(pacs: Set[Pac]) {
    val clashTarget = pacs.filter(pac => pac.clash && pac.needTarget)
    if (clashTarget.nonEmpty) {
      val minPac = clashTarget.minBy(_.pacId)
      //      Console.err.println(s"minPac: ${minPac.pacId} ${minPac.targetType} ${minPac.target}")
      minPac.setTarget(minPac.back)
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
      pac.setTarget(oppSquare)
    } else {
      canEatMe match {
        case Some((_, oppset)) =>  {
          val oppSq = oppset.head
//          Console.err.println(s"(${pac.x},${pac.y}) cannotEatOpp and go: opp=$oppSq target=${pac.target}")
          val squares = game.inMoves(pac.x, pac.y, 1) + board(pac.x)(pac.y)
          val newSq =  squares.maxBy(sq => Calc.euclidean(sq, oppSq))
          pac.setTarget(newSq)
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

    val newTarget = canEatOpp match {
      case Some((_, oppset)) =>  {
//        Console.err.println(s"(${pac.x},${pac.y}) canEatOpp: ${oppset.head} target=${pac.target}")
        oppset.head
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
    setvalTarget(pacs, squares.filter(_.prise == 10), dummy)                                  // prise = 10
    setSingleTarget(pacs, sureSquares.filter(_.prise == 1), onlinePrise)                      // visible prise on the same line
    setSingleTarget(pacs, unvisitedSquares, dummy)                                            // unvisited squares
  }

  def setvalTarget(pacs: Set[Pac], squares: Set[Square], f: (Set[Square], Pac) => Set[Square]) {
    val squaresPacs = pacs.map(pac => (board(pac.x)(pac.y), pac))
    val cartesian = squaresPacs.flatMap(squarePac => squares.map(priseSquare => (priseSquare, squarePac._1, squarePac._2 , Calc.euclidean(priseSquare, squarePac._1))))
    var sortedData = cartesian.toList.sortBy(_._4)
    while (sortedData.nonEmpty) {
      val pair = sortedData.head
      pair._3.setTarget(pair._1)
      sortedData = sortedData.filterNot(data => data._1 == pair._1 || data._3 == pair._3)
//      Console.err.println(s"$sortedData")
    }
  }

  def setSingleTarget(pacs: Set[Pac], squares: Set[Square], f: (Set[Square], Pac) => Set[Square]): Set[Square] = {
    var targetSquare = squares
    pacs.filter(_.needTarget).foreach(pac => {
      val filteredSquare = f(targetSquare, pac)
      if (filteredSquare.nonEmpty) {
        val square = filteredSquare.minBy(square => Calc.euclidean((square.x, square.y), (pac.x, pac.y)))
        targetSquare = targetSquare - square
        pac.setTarget(square)
      }
    })
    targetSquare
  }

  def calculateTargets(mypacset: Set[Pac], oppacset: Set[Pac], sureSquares: Set[Square], unvisitedSquares: Set[Square], game: Game) {
    resolveMyClash(mypacset)
    resolveEnemies(mypacset, oppacset, game)
    findSquare(mypacset, sureSquares, unvisitedSquares)
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
        pac.needTarget = true
        pac.clash = pac.x == x && pac.y == y && pac.prevaction == "MOVE"
        if (!pac.clash) pac.back = board(pac.x)(pac.y)
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

    squares.foreach(square => if (square.prise == 10) square.prise = 0)

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


    calculateTargets(mypacset, opppacset, surePelet.toSet, unvisitedSquares, game)
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