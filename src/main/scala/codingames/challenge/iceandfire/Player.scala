//package codingames.challenge.iceandfire

import math._
import scala.util._

/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/
object Player extends App {

  def printBoard(board: Array[String]) = board.foreach(Console.err.println)

  val numberminespots = readInt
  Console.err.println(s"numberminespots=$numberminespots")

  for (i <- 0 until numberminespots) {
    val Array(x, y) = for (i <- readLine split " ") yield i.toInt
    Console.err.println(s"\t($x,$y)")
  }

  // game loop
  while (true) {
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

    // Write an action using println
    // To debug: Console.err.println("Debug messages...")

    println("WAIT")
  }
}