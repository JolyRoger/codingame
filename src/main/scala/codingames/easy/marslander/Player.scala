//package codingames.easy.marslander

import math._
import scala.util._

/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/
object Player extends App {
  val surfacen = readInt // the number of points used to draw the surface of Mars.
  val data = for (i <- 0 until surfacen; coord = for (coordData <- readLine split " ") yield coordData.toInt) yield coord
  val dataReverse = data.reverse
  // landx: X coordinate of a surface point. (0 to 6999)
  // landy: Y coordinate of a surface point. By linking all the points together in a sequential fashion, you form the surface of Mars.
  //        val Array(landx, landy) = for(i <- readLine split " ") yield i.toInt

//  data.foreach(c => Console.err.println(s"x=${c.mkString("[", ":", "]")}"))
//  dataReverse.foreach(c => Console.err.println(s"xRev=${c.mkString("[", ":", "]")}"))
  //    data.takeWhile()
  def gravity = 3.711f
  def powers = List.range(0, 5)
  def nextPowers(currentPower: Int) = if (currentPower == 0) List(0, 1) else if (currentPower == 4) List(3, 4)
    else List(currentPower - 1, currentPower, currentPower + 1)
  def findNextPower(entered: Int, current: Int) = if (Math.abs(entered - current) > 1) {
      if (entered > current) current + 1 else current - 1
    } else entered


  def findThrustPower(ground: Int, y: Int, vspeed: Int, currentFuel: Long, currentPower: Int) = {
    def calculateNextState(power: Int) = {
      val newFuel = currentFuel - power
      val newVspeed = Math.round(vspeed - gravity + power)
      val newY = y + newVspeed
      (newVspeed, newY, newFuel)
    }
    Console.err.println(s"vspeed=$vspeed y=$y fuel=$currentFuel power=$currentPower")
    calculateNextState(currentPower)
  }

  var i = 0
  var flag = true

  while (true) {
    val Array(x, y, hspeed, vspeed, fuel, rotate, power) = for (i <- readLine split " ") yield i.toInt
    if (vspeed < -40) flag = false
    val tp = if (flag) 0 else if (vspeed < -39) 4 else 3
    val yLevel = dataReverse.dropWhile(coord => coord(0) > x).head(1)
    val (newSpeed, newY, newFuel) = findThrustPower(yLevel, y, vspeed, fuel, findNextPower(tp, power))
    Console.err.println(s"newspeed=$newSpeed newY=$newY newfuel=$newFuel")
    println("0 " + tp/* + (i % 5)*/)
    i = i + 1
  }
}