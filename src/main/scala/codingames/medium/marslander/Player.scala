package codingames.medium.marslander

import scala.io.StdIn._

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Player extends App {

  def findLandingArea(yPoints: Array[Array[Int]]): Array[(Int, Int)] = ???

  val surfaceN = readLine.toInt // the number of points used to draw the surface of Mars.
  Console.err.println(s"$surfaceN")

  val initData = (for (i <- 0 until surfaceN) yield (readLine split " ").map(_.toInt)).toArray

  for (i <- 0 until surfaceN) Console.err.println(s"${initData(i)(0)} ${initData(i)(1)}")

  initData

  // game loop
  while (true) {
    // hSpeed: the horizontal speed (in m/s), can be negative.
    // vSpeed: the vertical speed (in m/s), can be negative.
    // fuel: the quantity of remaining fuel in liters.
    // rotate: the rotation angle in degrees (-90 to 90).
    // power: the thrust power (0 to 4).
    val Array(x, y, hSpeed, vSpeed, fuel, rotate, power) = (readLine split " ").map(_.toInt)
    Console.err.println(s"$x $y $hSpeed $vSpeed $fuel $rotate $power")

    // Write an action using println
    // To debug: Console.err.println("Debug messages...")


    // rotate power. rotate is the desired rotation angle. power is the desired thrust power.
    println("20 2")
  }
}