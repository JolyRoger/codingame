package codingames.hard.voxcodei

import math._
import scala.util._
import scala.io.StdIn._

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Player extends App {
  // width: width of the firewall grid
  // height: height of the firewall grid
  val Array(width, height) = (readLine split " ").filter(_ != "").map (_.toInt)
  Console.err.println(s"$width $height")

  for(i <- 0 until height) {
    val mapRow = readLine // one line of the firewall grid
    Console.err.println(s"$mapRow")
  }

  // game loop
  while(true) {
    // rounds: number of rounds left before the end of the game
    // bombs: number of bombs left
    val Array(rounds, bombs) = (readLine split " ").filter(_ != "").map (_.toInt)
    Console.err.println(s"$rounds $bombs")
    // Write an action using println
    // To debug: Console.err.println("Debug messages...")

    println("3 1")
  }
}