package codingames.challenge.pikaptcha.l2

import scala.io.StdIn._

/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/
object Player extends App {
  val Array(width, height) = for (i <- readLine split " ") yield i.toInt
  Console.err.println(s"$width $height")
  for (i <- 0 until height) {
    val line = readLine
    Console.err.println(s"$line")
  }

  val side = readLine
  for (i <- 0 until height) {

    // Write an action using println
    // To debug: Console.err.println("Debug messages...")

    println("#####")
  }
}