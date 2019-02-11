package codingames.easy.marslander

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
  def g = 3.711f
  def powers = List.range(0, 5)
  def nextPowers(currentPower: Int) = if (currentPower == 0) List(0, 1) else if (currentPower == 4) List(3, 4)
    else List(currentPower - 1, currentPower, currentPower + 1)
  def findNextPower(entered: Int, current: Int) = if (Math.abs(entered - current) > 1) {
      if (entered > current) current + 1 else current - 1
    } else entered

  def timeOfFall(h: Int, y: Int, g: Float, thrust: Int)= sqrt(2 * (h - y) / (g - thrust))
  def speedInPoint(h: Int, g: Float, y: Int, thrust: Int)=  sqrt(2 * (h - y) * (g - thrust))
  def speedOfGround(h: Int, g: Float, thrust: Int)= speedInPoint(h, g, 100, thrust)
  def afterSec(h: Int, g: Float, t: Int)= h - g * t * t / 2

  def findThrustPower(ground: Int, y: Int, vspeed: Int, currentFuel: Int, currentPower: Int) = {
    def calculateNextState(power: Int) = {
      val newFuel = currentFuel - power
//      val newVspeed = round(vspeed - g + power)
      val newVspeed = round(vspeed - (g - power))
      val newY = round(y + (vspeed - (g - power) / 2))
      (newVspeed, newY, newFuel)
    }
    Console.err.println(s"vspeed=$vspeed y=$y fuel=$currentFuel power=$currentPower")
    calculateNextState(currentPower)
  }

  def calcNext(y: Int, vspeed: Int, fuel: Int, power: Int) = {
    val newFuel = fuel - power
    val newVspeed = round(vspeed - (g - power))
    val newY = round(y + (vspeed - (g - power) / 2))
    Console.err.println(s"DATA: y=$y power=$power\t${(newVspeed, newY, newFuel, power)}")
    (newVspeed, newY, newFuel, power)
  }

  // сколько топлива надо потратить при данных условиях чтобы достичь земли
  def findFuel(ground: Int, vspeed: Int, y: Int, fuel: Int, power: Int, acc: Int): Int = {
    if (y <= ground) if (vspeed < -40) 1000 else power else {
      val powers = nextPowers(power)
      val nextStates = powers.map(p => calcNext(y, vspeed, fuel, p)).filter(_._2 < y)
      val minNextState = nextStates.minBy(state => state._4 + findFuel(ground, state._1, state._2, state._3, state._4, acc))
      Console.err.print(s"$power+${minNextState._4} ")
      acc + power + minNextState._4
    }
  }

  var i = 0
  var flag = true

  while (true) {
    val Array(x, y, hspeed, vspeed, fuel, rotate, power) = for (i <- readLine split " ") yield i.toInt
    if (vspeed < -40) flag = false
    val tp = if (flag) 0 else if (vspeed < -39) 4 else 3
    val yLevel = dataReverse.dropWhile(coord => coord(0) > x).head(1)
    val (newSpeed, newY, newFuel) = findThrustPower(yLevel, y, vspeed, fuel, findNextPower(/*tp*/3, power))
    Console.err.println(s"newspeed=$newSpeed newY=$newY newfuel=$newFuel")
//    println("0 " + tp/* + (i % 5)*/)
    Console.err.println(s"$i sec passed")
    println("0 " + 3)
    i = i + 1
  }
}