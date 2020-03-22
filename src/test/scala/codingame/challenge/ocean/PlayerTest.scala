package codingame.challenge.ocean

import codingames.challenge.ocean.Player.readLine
import codingames.challenge.ocean.{Player, Square, SquareManager}
import org.scalatest.{BeforeAndAfter, FlatSpec}

import scala.io.Source
import scala.util.Random

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

  val legalSquares = board.flatten.filter(_.accessible)
  val rand = new Random(System.currentTimeMillis)
  val randSquare = legalSquares(rand.nextInt(legalSquares.length))

  board.foreach(bl => {
    bl.foreach(_.print)
    Console.err.println})

  // Write an action using println
  // To debug: Console.err.println("Debug messages...")

  println(s"${randSquare.getX} ${randSquare.getY}")
  randSquare.accessible = false
  val manager = new SquareManager(randSquare, board)

//======================================================================================================================
  "A Player" should "run application" in {
    Player.main(Array.empty[String])
  }

  "A SquareManager" should "find possible directions" in {
    val sm = new SquareManager(/*randSquare*/board(13)(10), board)
    val directions = sm.possibleDirection
    Console.err.println(s"${directions.mkString(",")}")
  }
}
