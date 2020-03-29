package codingame.challenge.ocean

import codingames.challenge.ocean.{MySquareManager, OppSquareManager, Player, Square}
import org.scalatest.{BeforeAndAfter, FlatSpec}

import scala.io.Source
import scala.util.Random

class PlayerTest extends FlatSpec with BeforeAndAfter {

//------------------------------------------FILE ENTRY------------------------------------------------------------------
  val filename = "ocean/ocean1.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines

  def readInt = if (data.hasNext) data.next.toInt else -1
  def readLine = if (data.hasNext) data.next else "EOF"
//------------------------------------------FILE ENTRY------------------------------------------------------------------

  val Array(width, height, myId) = (readLine split " ").map(_.toInt)
  Console.err.println(s"myId=$myId size=$width:$height")

  val boardSym = (for (i <- 0 until height) yield readLine).map(_.toCharArray)
  val board = boardSym.zipWithIndex.map(arrIndex => arrIndex._1.zipWithIndex.map(symIndex => new Square(symIndex._2, arrIndex._2, symIndex._1))).toArray

  board.foreach(bl => {
    bl.foreach(_.print)
    Console.err.println})

  // Write an action using println
  // To debug: Console.err.println("Debug messages...")


//======================================================================================================================
  "A Player" should "run application" in {
    Player.main(Array.empty[String])
  }

  "A SquareManager" should "find possible directions" in {
    val sm = new MySquareManager(board)
    val directions = sm.possibleDirection
    Console.err.println(s"${directions.mkString(",")}")
  }

  "A SquareManager" should "find torpedo squares" in {
    val torpedoSquares = new MySquareManager(board).safeTorpedoSquares
    Console.err.println(s"${torpedoSquares.mkString(",")}")
  }

  "An OppSquareManager" should "return correct next square" in {
    val myManager = new MySquareManager(board)
    val oppManager = new OppSquareManager(myManager.legalSquares, myManager.coordSquaresMap)
    val res = oppManager.nextSquare(myManager.coordSquaresMap((2,2)), "N")
    Console.err.println(s"$res")
  }

  "An OppSquareManager" should "find legal move enemy squares" in {
    val path = Array("E", "E", "S", "E", "E", "E", "E", "E", "E", "E", "E", "E", "E", "N", "E", "E")
    val myManager = new MySquareManager(board)
    val oppManager = new OppSquareManager(myManager.legalSquares, myManager.coordSquaresMap)
    val oppLegalSquaresMap = myManager.legalSquares.map((_, List.empty[Square])).toMap

    var leg = oppLegalSquaresMap

    for (direction <- path) {
      leg = oppManager.processOpponentMove(s"MOVE $direction", leg)
    }

    Console.err.println(s"${leg.keys.mkString(",")}")
  }

  "An OppSquareManager" should "find legal surface enemy squares" in {
    val myManager = new MySquareManager(board)
    val oppManager = new OppSquareManager(myManager.legalSquares, myManager.coordSquaresMap)
    val oppLegalSquaresMap = myManager.legalSquares.map((_, List.empty[Square])).toMap
    val moveWest = oppManager.processOpponentMove(s"MOVE E", oppLegalSquaresMap)
    val moveWest2 = oppManager.processOpponentMove(s"MOVE E", moveWest)
    val surface = oppManager.processOpponentSurface(s"SURFACE 1", moveWest2)
    Console.err.println(s"MOVE::${moveWest2.keys.mkString(" ")}")
    Console.err.println(s"SURF::${surface.keys.mkString(" ")}")
  }
}
