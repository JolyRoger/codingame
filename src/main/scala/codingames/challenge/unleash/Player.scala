//package codingames.challenge.unleash

import math._
import scala.util._

class Robot(val i: Int, var x: Int, var y: Int, var item: Int) {
  var command = "WAIT"
  var isFlying = false
  var targetX: Int = 0
  var targetY: Int = 0
  def near =
    x == targetX && y == targetY ||
    x == targetX - 1 && y == targetY ||
    x == targetX + 1 && y == targetY ||
    x == targetX && y == targetY - 1 ||
    x == targetX && y == targetY + 1

  def getCommand = if (command =="DIG" || command =="MOVE")s"$command $targetX $targetY" else command
  def print = Console.err.println(s"$i: ($x, $y) - $item")
}

object RobotManager {

  var radarRequested = false
  var trapRequested = false

  def dig(mapDataMap: Map[(Int, Int), (String, Int)], robot: Robot) {
    val coords = mapDataMap.withFilter(entity => entity._2._2 == 0).map(_._1).filter(c => c._1 != 0).toArray
    val coordRand = coords(Random.nextInt(coords.length))
    robot.command = "DIG"
    robot.targetX = coordRand._1
    robot.targetY = coordRand._2
    robot.isFlying = true
  }

  def enrich(robot: Robot,
             mapDataMap: Map[(Int, Int), (String, Int)],
             entityData: List[Array[Int]]) = {
//    Console.err.println(s"BEFORE COMMAND: ${robot.i}: (${robot.x},${robot.y}) ${robot.getCommand} / ${robot.isFlying}")
//    robot.isFlying = robot.targetX

    if (mapDataMap((robot.targetX, robot.targetY))._2 == 1 &&
      (mapDataMap((robot.targetX, robot.targetY))._1 == "?" || mapDataMap((robot.targetX, robot.targetY))._1 == "0")) {
      robot.isFlying = false
    }

    if (robot.x == 0) {
      robot.isFlying = false
    }

    if (robot.item == 4) {
      robot.command = "MOVE"
      robot.targetX = 0
      robot.targetY = robot.y
    } else if (robot.item == 2 || robot.item == 3) {
      if (!robot.isFlying) {
        dig(mapDataMap, robot)
      }
    } else if (robot.item == -1) {
      if (!robot.isFlying) {
//        Console.err.println(s"-1 SECTION :: BEFORE COMMAND: ${robot.i}: (${robot.x},${robot.y}) ${robot.getCommand} / ${robot.isFlying}")
//        Console.err.println(s"trapRequested:: $trapRequested\tradarRequested::$radarRequested")
        if (!radarRequested) {
          robot.command = "REQUEST RADAR"
          radarRequested = true
        } else if (!trapRequested) {
          robot.command = "REQUEST TRAP"
          trapRequested = true
        } else {
          dig(mapDataMap, robot)
        }
      }
    }
//    Console.err.println(s"AFTER COMMAND: ${robot.i}: (${robot.x},${robot.y}) ${robot.getCommand} / ${robot.isFlying}")
    robot
  }

  def command(robots: List[Robot],
              mapDataMap: Map[(Int, Int), (String, Int)],
              entityData: List[Array[Int]]) = {

//    Console.err.println(s"----------------- ROBOTS ------------------------------------------------")
//    robots.foreach(_.print)
    Console.err.println(s"----------------- ENTITY DATA -------------------------------------------")
    entityData.foreach(ed => {
      Console.err.println(s"[${ed(0)}: ${ed(1)}, (${ed(2)}, ${ed(3)}): ${ed(4)}], ")
    })
    Console.err.println(s"----------------- MAP DATA MAP -------------------------------------------")
//    mapDataMap.foreach(Console.err.println)
    Console.err.println(s"(11,12) ${mapDataMap(11, 12)}")
    Console.err.println(s"(12,8) ${mapDataMap(12, 8)}")
    Console.err.println(s"----------------- MAP DATA MAP -------------------------------------------")

    robots.foreach(enrich(_, mapDataMap, entityData))
    robots
  }
}

/**
 * Deliver more ore to hq (left side of the map) than your opponent. Use radars to find ore but beware of traps!
 **/
object Player extends App {
  def toMatrix(number: Int): (Int, Int) = (number / 30, number % 15)
  implicit def toNumber(point: (Int, Int)): Int = point._2 * 30 + point._1
  def oreHole(inputs: Array[String]) = {
    val res = inputs.zipWithIndex.partition(in => in._2 % 2 == 0)
    res._1.map(_._1).zip(res._2.map(_._1.toInt))/*.zipWithIndex*/
  }
  def createMapData(mapData: List[Array[(String, Int)]]) = {
    val mapDataMap = (for (i <- mapData.indices; j <- mapData(i).indices) yield ((j,i) -> mapData(i)(j))).toMap
    mapDataMap
  }

  var firstMove = true

  // height: size of the map
  val Array(width, height) = for (i <- readLine split " ") yield i.toInt
  Console.err.println(s"width=$width\theight=$height")

  var myRobotsRaw: List[Robot] = List.empty
  var myRobotsMap: Map[Int, Robot] = Map.empty

  // game loop
  while (true) {
    // myscore: Amount of ore delivered
    val Array(myscore, opponentscore) = for (i <- readLine split " ") yield i.toInt
    Console.err.println(s"myscore=$myscore\topponentscore=$opponentscore")

    val mapData = (for (i <- 0 until height) yield {
      val inputsStr = readLine
//      Console.err.println(s"inputsStr=$inputsStr")
      val inputs = inputsStr split " "
      oreHole(inputs)
/*
      for (j <- 0 until width) {
        // ore: amount of ore or "?" if unknown
        // hole: 1 if cell has a hole
        val hole = inputs(2 * j + 1).toInt
        Console.err.print(s"$hole ")
      }
      Console.err.println
*/
    }).toList

    val mapDataMap = createMapData(mapData)
    Console.err.println("----------------------------------")

    // entitycount: number of entities visible to you
    // radarcooldown: turns left until a new radar can be requested
    // trapcooldown: turns left until a new trap can be requested
    val Array(entitycount, radarcooldown, trapcooldown) = for (i <- readLine split " ") yield i.toInt
    Console.err.println(s"entitycount=$entitycount radarcooldown=$radarcooldown trapcooldown=$trapcooldown")

    val entityData = (for (i <- 0 until entitycount) yield
      // id: unique id of the entity
      // sort: 0 for your robot, 1 for other robot, 2 for radar, 3 for trap
      // y: position of the entity
      // item: if this entity is a robot, the item it is carrying (-1 for NONE, 2 for RADAR, 3 for TRAP, 4 for ORE)
      /*val Array(id, sort, x, y, item) = */for (i <- readLine split " ") yield i.toInt).toList
//      Console.err.println(s"id=$id sort=$sort x=$x y=$y item=$item")

    val myRobotsData = entityData.filter(data => data(1) == 0)
    val myRobotsDataMap = myRobotsData.map(data => data(0) -> data).toMap

    if (firstMove) {
      myRobotsRaw = myRobotsData.map(dataset => new Robot(dataset(0), dataset(2), dataset(3), dataset(4)))
      myRobotsMap = myRobotsRaw.map(robot => robot.i -> robot).toMap
    } else {
      myRobotsRaw.foreach(robot => {
        val data = myRobotsDataMap(robot.i)
        robot.x = data(2)
        robot.y = data(3)
        robot.item = data(4)
      })
    }

    val myRobots = RobotManager.command(myRobotsRaw, mapDataMap, entityData)


    if (myRobots.length < 5) Console.err.println(s"ROBOT IS DEAD!")

    for (i <- 0 until 5) {
      // Write an action using println
      // To debug: Console.err.println("Debug messages...")

      println(myRobotsMap(i).getCommand) // WAIT|MOVE x y|DIG x y|REQUEST item
    }

    firstMove = false
  }
}