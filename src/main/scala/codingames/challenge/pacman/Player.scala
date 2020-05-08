package codingames.challenge.pacman

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
        matrix.head.indices.map(i => matrix.map(_(i))).toArray
    }


    def show(squares: Set[Square]): Unit = {
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
}
class Pac(val pacId: Int, val mine: Boolean, var x: Int, var y: Int, val typeId: String, var speedTurnsLeft: Int, var abilityCooldown: Int) {
    var target: Square = _
    def command = s"MOVE ${pacId} ${target.x} ${target.y}"
}





/**
 * Grab the pellets as fast as you can!
 **/
object Player extends App {
    def linesToBoard(lines: List[String]): Board = new Board(lines, ' ', '#')
    def needTarget(pac: Pac) = pac.target == null ||
      (pac.target.x == pac.x && pac.target.y == pac.y) ||
      pac.target.prise == 0

    def setTarget(pacs: Array[Pac], squares: Set[Square]) {
        var priseMap = squares.groupBy(_.prise) - 0

        for (pac <- pacs; if pac != null) {
            pac.target = if (needTarget(pac)) {
                val maxPrise = priseMap.keySet.max
                val targetSet = priseMap(maxPrise)
                val square = targetSet.minBy(square => Calc.euclidean((square.x, square.y), (pac.x, pac.y)))
                priseMap += (maxPrise -> (targetSet - square))
                square
            } else pac.target
        }
    }

    //------------------------------------------FILE ENTRY------------------------------------------------------------------
//      val filename = "resources/pacman/pacman1.txt"
//      val bufferedSource = Source.fromFile(filename)
//      val data = bufferedSource.getLines
//      def readInt = if (data.hasNext) data.next.toInt else -1
//      def readLine = if (data.hasNext) data.next else "EOF"
    //----------------------------------------------------------------------------------------------------------------------


    // width: size of the grid
    // height: top left corner is (x=0, y=0)
    val Array(width, height) = (readLine split " ").map (_.toInt)
//    Console.err.println(s"${width} $height")
    val lines = (for(i <- 0 until height) yield readLine).toList
    val board = linesToBoard(lines)
    val squares = board.toSet
//    var pacmap = Array.fill[Pac](10)(null)
    var mypac = Array.fill[Pac](5)(null)
    var oppac = Array.fill[Pac](5)(null)

    // game loop
    while(true) {
        val Array(myScore, opponentScore) = (readLine split " ").map (_.toInt)
//        Console.err.println(s"$myScore $opponentScore")
        val visiblePacCount = readLine.toInt // all your pacs and enemy pacs in sight
//        Console.err.println(s"$visiblePacCount")

        for(i <- 0 until visiblePacCount) {
            // pacId: pac number (unique within a team)
            // mine: true if this pac is yours
            // x: position in the grid
            // y: position in the grid
            // typeId: unused in wood leagues
            // speedTurnsLeft: unused in wood leagues
            // abilityCooldown: unused in wood leagues
            val Array(_pacId, _mine, _x, _y, typeId, _speedTurnsLeft, _abilityCooldown) = readLine split " "
            val pacId = _pacId.toInt
            val mine = _mine.toInt != 0
            val x = _x.toInt
            val y = _y.toInt
            val speedTurnsLeft = _speedTurnsLeft.toInt
            val abilityCooldown = _abilityCooldown.toInt
            val targetPac = if (mine) mypac else oppac
            if (targetPac(pacId) == null) targetPac(pacId) = new Pac(pacId, mine, x, y, typeId, speedTurnsLeft, abilityCooldown)
            else {
                targetPac(pacId).x = x
                targetPac(pacId).y = y
                targetPac(pacId).speedTurnsLeft = speedTurnsLeft
                targetPac(pacId).abilityCooldown = abilityCooldown
            }
//            Console.err.println(s"${_pacId} $mine ${_x} ${_y} $typeId ${_speedTurnsLeft} ${_abilityCooldown}")
        }

        val visiblePelletCount = readLine.toInt // all pellets in sight
//        Console.err.println(s"$visiblePelletCount")
        squares.foreach(_.prise = 0)
        for(i <- 0 until visiblePelletCount) {
            // value: amount of points this pellet is worth
            val Array(x, y, value) = (readLine split " ").map (_.toInt)
            board(x)(y).prise = value
            //            Console.err.println(s"$x $y $value")
        }

        setTarget(mypac, squares)

        mypac.filterNot(_ == null).foreach { pac =>
            println(s"${pac.command}") // MOVE <pacId> <x> <y>
        }
    }
}