package codingames.challenge.ocean

import math._
import scala.io.Source
import scala.util._
import scala.io.StdIn._

class Square(x: Int, y: Int, sym: Char) {
  val getX = x
  val getY = y
  val getSym = sym
  val water = sym == '.'
  var accessible = sym == '.'

  val allTorpedoSquares = calcSquare(x, y)

  private def calc(x: Int, y: Int, offset: Int, leftLimit: Int, rightLimit: Int) = {
    val internalOffset = 4 - Math.abs(offset)
    val a = (x - internalOffset to x).dropWhile(_ < leftLimit).toSet
    val b = (x to x + internalOffset).takeWhile(_ <= rightLimit).toSet
    (a ++ Set(x) ++ b).map((_, y - offset)).filter(pair => pair._2 >= 0 && pair._2 < 15)
  }

  private def calcSquare(x: Int, y: Int) = {
    (for (offset <- -4 to 4) yield calc(x, y, offset, 0, 14)).toSet.flatten
  }

  def print = Console.err.print(s"$sym")
  override def toString = s"($x,$y)$sym"
}

class SquareManager(board: Array[Array[Square]]) {
  val flattenBoard = board.flatten
  val coordSquaresMap = flattenBoard.map(square => ((square.getX, square.getY), square)).toMap
  val legalSquares = flattenBoard.filter(_.accessible)
  val rand = new Random(System.currentTimeMillis)
//  var myPosition = coordSquaresMap((0, 8))
  var myPosition = legalSquares(rand.nextInt(legalSquares.length))
  Console.err.println(s"my position=$myPosition")

  val torpedoSquareMap = legalSquares.map(square => (square, square.allTorpedoSquares.filter(coordSquaresMap(_).water))).toMap
  val safeTorpedoSquareMap = torpedoSquareMap.map(kv => (kv._1, kv._2.filter { xy =>
    (Math.abs(kv._1.getX - xy._1) > 1 || Math.abs(kv._1.getY - xy._2) > 1) && coordSquaresMap(xy).water
  }))
  myPosition.accessible = false

  def safeTorpedoSquares = safeTorpedoSquareMap(myPosition)

  def surface = {
    legalSquares.foreach(square => square.accessible = true)
    myPosition.accessible = false
    ""
  }

  def setMyPosition(square: Square) = {
    myPosition = square
    myPosition.accessible = false
  }


  def possibleDirection = {
    val candidates = Array((myPosition.getX + 1, myPosition.getY, 'E'), (myPosition.getX - 1, myPosition.getY, 'W'), (myPosition.getX, myPosition.getY + 1, 'S'), (myPosition.getX, myPosition.getY - 1, 'N'))
    candidates.filter(xy => xy._1 >= 0 && xy._1 < 15 &&
                            xy._2 >= 0 && xy._2 < 15 &&
                            board(xy._2)(xy._1).accessible)
      .map(_._3)
  }


}

object Player extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
//  val filename = "ocean/ocean0.txt"
//  val bufferedSource = Source.fromFile(filename)
//  val data = bufferedSource.getLines
//
//  def readInt = if (data.hasNext) data.next.toInt else -1
//  def readLine = if (data.hasNext) data.next else "EOF"
//----------------------------------------------------------------------------------------------------------------------

  val Array(width, height, myId) = (readLine split " ").map(_.toInt)
  Console.err.println(s"myId=$myId size=$width:$height")

  val boardSym = (for (i <- 0 until height) yield readLine).map(_.toCharArray)
  val board = boardSym.zipWithIndex.map(arrIndex => arrIndex._1.zipWithIndex.map(symIndex => new Square(symIndex._2, arrIndex._2, symIndex._1))).toArray
  val manager = new SquareManager(board)

  board.foreach(bl => {
    bl.foreach(_.print)
    Console.err.println})

  println(s"${manager.myPosition.getX} ${manager.myPosition.getY}")

  // game loop
  while (true) {
    val Array(x, y, myLife, oppLife, torpedoCooldown, sonarCooldown, silenceCooldown, mineCooldown) = (readLine split " ").map(_.toInt)
    manager.setMyPosition(board(y)(x))
    Console.err.println(s"$x $y $myLife $oppLife $torpedoCooldown $sonarCooldown $silenceCooldown $mineCooldown")
    val sonarResult = readLine
    Console.err.println(s"sonarResult=$sonarResult")
    val opponentOrders = readLine
    Console.err.println(s"opponentOrders=$opponentOrders")

    val directions = manager.possibleDirection
    val mainCommand = if (directions.isEmpty) "SURFACE" else s"MOVE ${directions(manager.rand.nextInt(directions.length))} TORPEDO"

    val dopCommand = if (mainCommand == "SURFACE") {
      manager.surface
    } else if (mainCommand.startsWith("MOVE")) {
      s"${if (torpedoCooldown > 0) "" else {
        val torpedoSquares = manager.safeTorpedoSquares
        torpedoSquares.find(_ => true) match {
          case Some(s) => {
            s"|TORPEDO ${s._1} ${s._2}"
          }
          case None => ""
        }
      }}"
    }

    println(s"$mainCommand$dopCommand")
  }
}