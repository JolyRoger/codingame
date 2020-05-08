package codingame.challenge.ocean

import codingames.challenge.ocean.Player
import codingames.challenge.ocean.Player._
import org.scalatest.{BeforeAndAfter, FlatSpec}

import scala.io.Source

class PlayerTest extends FlatSpec with BeforeAndAfter {

//------------------------------------------FILE ENTRY------------------------------------------------------------------
  val filename = "ocean/ocean0.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines

  def readInt = if (data.hasNext) data.next.toInt else -1
  def readLine = if (data.hasNext) data.next else "EOF"
//------------------------------------------FILE ENTRY------------------------------------------------------------------

  val Array(width, height, myId) = (readLine split " ").map(_.toInt)
  Console.err.println(s"myId=$myId size=$width:$height")

  val boardSym = (for (i <- 0 until height) yield readLine).map(_.toCharArray)
  val board = boardSym.zipWithIndex.map(arrIndex => arrIndex._1.zipWithIndex.map(symIndex => new Square(symIndex._2, arrIndex._2, symIndex._1))).toArray
  val flattenBoard = board.flatten
  val squareArray = flattenBoard
  val coordSquaresMap = flattenBoard.map(square => ((square.getX, square.getY), square)).toMap
  val legalSquares = flattenBoard.filter(_.water)

  var euclideanDistanceMap = Map.empty[(Square, Square), Double]

  for (square1 <- legalSquares; square2 <- legalSquares; if !euclideanDistanceMap.contains((square1, square2))) {
    val dist = euclidean((square1.getX, square1.getY), (square2.getX, square2.getY))
    euclideanDistanceMap = euclideanDistanceMap + ((square1, square2) -> dist)
    euclideanDistanceMap = euclideanDistanceMap + ((square2, square1) -> dist)
  }

  val myManager = new MySquareManager(board, legalSquares, flattenBoard, euclideanDistanceMap)
  val oppManager = new OppSquareManager(board, legalSquares, flattenBoard)

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
    val directions = myManager.possibleDirection
    Console.err.println(s"${directions.mkString(",")}")
  }

  "A SquareManager" should "find torpedo squares" in {
    Console.err.println(s"pos=${myManager.myPosition}")
    val torpedoSquares = myManager.safeTorpedoSquareMap(myManager.myPosition)
    Console.err.println(s"${torpedoSquares.mkString(",")}")
    assert(torpedoSquares.equals(Set(board(2)(0))))
  }

  "An OppSquareManager" should "return correct next square" in {
    val res = oppManager.nextRawSquareTry(coordSquaresMap((2,2)), "N")
    Console.err.println(s"$res")
  }

  "An OppSquareManager" should "find legal move enemy squares" in {
    val path = Array("E", "E", "S", "E", "E", "E", "E", "E", "E", "E", "E", "E", "E", "N", "E", "E")
    val oppLegalSquaresMap = legalSquares.map((_, List.empty[PathInfo])).toMap
    var leg = oppLegalSquaresMap

    for (direction <- path) {
      leg = oppManager.processOpponentMove(s"MOVE $direction", leg)
    }

    Console.err.println(s"${leg.mkString(",")}")
  }

  "An OppSquareManager" should "find legal surface enemy squares" in {
    val oppLegalSquaresMap = legalSquares.map((_, List.empty[PathInfo])).toMap
    val moveWest = oppManager.processOpponentMove(s"MOVE E", oppLegalSquaresMap)
    val moveWest2 = oppManager.processOpponentMove(s"MOVE E", moveWest)
    val surface = oppManager.processOpponentSurface(s"SURFACE 1", moveWest2)
    Console.err.println(s"MOVE::${moveWest2.mkString(" ")}")
    Console.err.println(s"SURF::${surface.mkString(" ")}")
  }

  "A OppSquareManager" should "process silence command" in {
    val oppLegalSquaresMap = legalSquares.map((_, List.empty[PathInfo])).toMap
    val surfaceMove = oppManager.processOpponentSurface(s"SURFACE 9", oppLegalSquaresMap)
    val moveMove = oppManager.processOpponentMove(s"MOVE W", surfaceMove)
    val silenceMove = oppManager.processOpponentSilence(moveMove)

    Console.err.println(s"silenceMove.size=${silenceMove.size}")

    surfaceMove.keys.toList.sortBy(_.index).foreach(Console.err.print)
    Console.err.println
    silenceMove.keys.toList.sortBy(_.index).foreach(Console.err.print)
  }
}
