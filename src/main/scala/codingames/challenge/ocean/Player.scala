//package codingames.challenge.ocean

import math._
import scala.io.Source
import scala.util._
import scala.io.StdIn._


class Square(x: Int, y: Int, sym: Char) {
  val getX = x
  val getY = y
  val getSym = sym
  def print = Console.err.print(s"$x:$y:$sym ")
}

class SquareManager(var myPosition: Square, board: Array[Array[Square]]) {
  def possibleDirection = ee
}

object Player extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
/*
  val filename = "ocean/ocean0.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines

  def readInt = if (data.hasNext) data.next.toInt else -1
  def readLine = if (data.hasNext) data.next else "EOF"
*/
//----------------------------------------------------------------------------------------------------------------------

  val Array(width, height, myId) = (readLine split " ").map(_.toInt)
  Console.err.println(s"myId=$myId size=$width:$height")

  val boardSym = (for (i <- 0 until height) yield readLine).map(_.toCharArray)
  val board = boardSym.zipWithIndex.map(arrIndex => arrIndex._1.zipWithIndex.map(symIndex => new Square(symIndex._2, arrIndex._2, symIndex._1))).toArray

  val legalSquares = board.flatten.filter(_.getSym == '.')
  val rand = new Random(System.currentTimeMillis)
  val randSquare = legalSquares(rand.nextInt(legalSquares.length))




//  board.foreach(bl => {
//    bl.foreach(_.print)
//    Console.err.println(s"\n")})

  // Write an action using println
  // To debug: Console.err.println("Debug messages...")

  println(s"${randSquare.getX} ${randSquare.getY}")
  val manager = new SquareManager(randSquare, board)

  // game loop
  while (true) {
    val Array(x, y, myLife, oppLife, torpedoCooldown, sonarCooldown, silenceCooldown, mineCooldown) = (readLine split " ").map(_.toInt)
    val sonarResult = readLine
    Console.err.println(s"sonarResult=$sonarResult")
    val opponentOrders = readLine
    Console.err.println(s"opponentOrders=$opponentOrders")

    // Write an action using println
    // To debug: Console.err.println("Debug messages...")

    println("MOVE N TORPEDO")
  }
}