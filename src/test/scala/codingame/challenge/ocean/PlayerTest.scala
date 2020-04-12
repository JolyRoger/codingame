package codingame.challenge.ocean

import codingames.challenge.ocean.Player
import codingames.challenge.ocean.Player.{MySquareManager, OppSquareManager, PathInfo, Square, oppSquares}
import org.scalatest.{BeforeAndAfter, FlatSpec}

import scala.io.Source
import scala.util.Random

class PlayerTest extends FlatSpec with BeforeAndAfter {

//------------------------------------------FILE ENTRY------------------------------------------------------------------
  val filename = "ocean/ocean4.txt"
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

  val myManager = new MySquareManager(board, legalSquares, flattenBoard)
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
    val torpedoSquares = myManager.safeTorpedoSquares
    Console.err.println(s"${torpedoSquares.mkString(",")}")
  }

  "An OppSquareManager" should "return correct next square" in {
    val res = oppManager.nextRawSquare(coordSquaresMap((2,2)))
    Console.err.println(s"$res")
  }

  "An OppSquareManager" should "find legal move enemy squares 2" in {
//    [7,0].[8,0].[9,0].[10,0].[11,0].[12,0].[13,0].[14,0].[0,1].[13,1].[14,1].[13,2].[14,2].[10,3].[11,3].[12,3].[13,3].[14,3].[0,4].[11,4].[12,4].[13,4].[14,4].[0,5].[11,5].[12,5].[13,5].[14,5].[0,6].
    val ls = Array(
      coordSquaresMap((7,0)),
      coordSquaresMap((8,0)),
      coordSquaresMap((9,0)),
      coordSquaresMap((10,0)),
      coordSquaresMap((11,0)),
      coordSquaresMap((12,0)),
      coordSquaresMap((13,0)),
      coordSquaresMap((14,0)),
      coordSquaresMap((0,1)),
      coordSquaresMap((13,1)),
      coordSquaresMap((14,1)),
      coordSquaresMap((13,2)),
      coordSquaresMap((14,2)),
      coordSquaresMap((10,3)),
      coordSquaresMap((11,3)),
      coordSquaresMap((12,3)),
      coordSquaresMap((13,3)),
      coordSquaresMap((14,3)),
      coordSquaresMap((0,4)),
      coordSquaresMap((11,4)),
      coordSquaresMap((12,4)),
      coordSquaresMap((13,4)),
      coordSquaresMap((14,4)),
      coordSquaresMap((0,5)),
      coordSquaresMap((11,5)),
      coordSquaresMap((12,5)),
      coordSquaresMap((13,5)),
      coordSquaresMap((14,5)),
      coordSquaresMap((0,6))
    ).map(s => (s, List.empty[(String, Square)])).toMap
    val res = oppManager.processOpponentMove(s"MOVE W", ls)
    res.keys.toList.sortBy(_.index).foreach(Console.err.println)
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

//    Console.err.println(s"surface::${surfaceMove.mkString(" ")}")
//    Console.err.println(s"silence::${silenceMove.mkString(" ")}")
  }
/*
  "A Graph" should "find path for all squares" in {
    val graph = new Graph(board, flattenBoard)
    val squareNum = 0
    val (edge, dist) = graph.allDistance(squareNum, _.water)
    val square = flattenBoard(squareNum)

    dist.zipWithIndex.filter(_._1 < Int.MaxValue).foreach(a => {
      Console.err.println(s"Distance from (${square.getX},${square.getY}) to (${flattenBoard(a._2).getX},${flattenBoard(a._2).getY}) is " +
        s"${if (a._1 < Int.MaxValue) a._1 else "unreachable"}")
    })
  }

  "A test" should "run" in {
    Console.err.println(s"aaa")
  }*/

}
