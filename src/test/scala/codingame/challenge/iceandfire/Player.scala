package codingame.challenge.iceandfire

import java.io.File

import math._
import scala.io.Source
import scala.util._

/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/
object Player extends App {
  val filename = "iceandfire0.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines
  val limit = 1
  var step = 0

  def readInt = if (data.hasNext) data.next.toInt else -1
  def readLine = if (data.hasNext) data.next else "\0"

  def printBoard(board: Array[String]) = board.foreach(Console.err.println)

//----------------------------------------------------------------------------------------------------------------------

  val numberminespots = readInt
  Console.err.println(s"numberminespots=$numberminespots")

  for (i <- 0 until numberminespots) {
    val Array(x, y) = for (i <- readLine split " ") yield i.toInt
    Console.err.println(s"\t($x,$y)")
  }


  // game loop
  while (step < limit) {
    val gold = readInt
    Console.err.println(s"")
    val income = readInt
    Console.err.println(s"my gold/income = [$gold/$income]")
    val opponentgold = readInt
    val opponentincome = readInt
    Console.err.println(s"opponent gold/income = [$opponentgold/$opponentincome]")
    val board = (for (i <- 0 until 12) yield readLine).toArray
    printBoard(board)

    val buildingcount = readInt
    Console.err.println(s"buildingcount=$buildingcount")
    for (i <- 0 until buildingcount) {
      val Array(owner, buildingtype, x, y) = for (i <- readLine split " ") yield i.toInt
      Console.err.println(s"\t[owner, type, x, y]: $owner $buildingtype ($x,$y)")
    }
    val unitcount = readInt
    Console.err.println(s"unitcount=$unitcount")
    for (i <- 0 until unitcount) {
      val Array(owner, unitid, level, x, y) = for (i <- readLine split " ") yield i.toInt
      Console.err.println(s"\t[owner, unitid, level, x, y]: $owner $unitid $level ($x,$y)")
    }

    step += 1
    // Write an action using println
    // To debug: Console.err.println("Debug messages...")

    println("WAIT")

  }
}