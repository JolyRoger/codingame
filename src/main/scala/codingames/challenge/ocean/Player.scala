//package codingames.challenge.ocean

import math._
import scala.util._
import scala.io.StdIn._

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Player extends App {
  val Array(width, height, myId) = (readLine split " ").map(_.toInt)
  Console.err.println(s"myId=$myId size=$width:$height")
  for (i <- 0 until height) {
    val line = readLine
    Console.err.println(s"line=$line")
  }

  // Write an action using println
  // To debug: Console.err.println("Debug messages...")

  println("0 13")

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