//package codingames.challenge.ocean

import math._
import scala.io.Source
import scala.util._
import scala.io.StdIn._


class Square(x: Int, y: Int, sym: Char) {
  val getX = x
  val getY = y
  val getSym = sym
  val rock = sym == 'x'
  var accessible = sym == '.'
  def print = Console.err.print(s"$sym")
//  def print = Console.err.print(s"$x:$y:$sym ")
}

class SquareManager(var myPosition: Square, board: Array[Array[Square]]) {

  myPosition.accessible = false

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

  val legalSquares = board.flatten.filter(_.accessible)
  val rand = new Random(System.currentTimeMillis)
  val randSquare = legalSquares(rand.nextInt(legalSquares.length))




//  board.foreach(bl => {
//    bl.foreach(_.print)
//    Console.err.println})

  // Write an action using println
  // To debug: Console.err.println("Debug messages...")

  println(s"${randSquare.getX} ${randSquare.getY}")
  val manager = new SquareManager(randSquare, board)

  // game loop
  while (true) {
    val Array(x, y, myLife, oppLife, torpedoCooldown, sonarCooldown, silenceCooldown, mineCooldown) = (readLine split " ").map(_.toInt)
    manager.setMyPosition(board(y)(x))
    Console.err.println(s"$x $y $myLife $oppLife $torpedoCooldown $sonarCooldown $silenceCooldown $mineCooldown")
    val sonarResult = readLine
    Console.err.println(s"sonarResult=$sonarResult")
    val opponentOrders = readLine
    Console.err.println(s"opponentOrders=$opponentOrders")

    // Write an action using println
    // To debug: Console.err.println("Debug messages...")
    val directions = manager.possibleDirection
    val index = rand.nextInt(directions.length)

    println(s"MOVE ${directions(index)} TORPEDO")
  }
}