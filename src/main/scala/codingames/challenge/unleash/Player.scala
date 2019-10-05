package codingames.challenge.unleash

import math._
import scala.util._

/**
 * Deliver more ore to hq (left side of the map) than your opponent. Use radars to find ore but beware of traps!
 **/
object Player extends App {

  def toMatrix(number: Int): (Int, Int) = (number / 30, number % 15)
  implicit def toNumber(point: (Int, Int)): Int = point._2 * 30 + point._1
  def oreHole(inputs: Array[String]) = {
    val res = inputs.zipWithIndex.partition(in => in._2 % 2 == 0)
    res._1.map(_._1).zip(res._2.map(_._1)).zipWithIndex
  }


  // height: size of the map
  val Array(width, height) = for (i <- readLine split " ") yield i.toInt
  Console.err.println(s"width=$width\theight=$height")

  // game loop
  while (true) {
    // myscore: Amount of ore delivered
    val Array(myscore, opponentscore) = for (i <- readLine split " ") yield i.toInt
    Console.err.println(s"myscore=$myscore\topponentscore=$opponentscore")

    for (i <- 0 until height) {
      val inputsStr = readLine
      Console.err.println(s"inputsStr=$inputsStr")
      var inputs = inputsStr split " "

      for (j <- 0 until width) {
        // ore: amount of ore or "?" if unknown
        // hole: 1 if cell has a hole
        val hole = inputs(2 * j + 1).toInt
        Console.err.print(s"$hole ")
      }
      Console.err.println
    }
    Console.err.println("----------------------------------")

    // entitycount: number of entities visible to you
    // radarcooldown: turns left until a new radar can be requested
    // trapcooldown: turns left until a new trap can be requested
    val Array(entitycount, radarcooldown, trapcooldown) = for (i <- readLine split " ") yield i.toInt
    Console.err.println(s"entitycount=$entitycount radarcooldown=$radarcooldown trapcooldown=$trapcooldown")

    for (i <- 0 until entitycount) {
      // id: unique id of the entity
      // sort: 0 for your robot, 1 for other robot, 2 for radar, 3 for trap
      // y: position of the entity
      // item: if this entity is a robot, the item it is carrying (-1 for NONE, 2 for RADAR, 3 for TRAP, 4 for ORE)
      val Array(id, sort, x, y, item) = for (i <- readLine split " ") yield i.toInt
      Console.err.println(s"id=$id sort=$sort x=$x y=$y item=$item")
    }

    for (i <- 0 until 5) {

      // Write an action using println
      // To debug: Console.err.println("Debug messages...")

      println("WAIT") // WAIT|MOVE x y|DIG x y|REQUEST item
    }
  }
}