package codingames.challenge.unleash

import math._
import scala.util._

class Square(x: Int, y: Int) {
  var hole: Boolean = false
  var radar: Int = -1  // 0 - not presented, 1 - on the place, -1 - unknown
  var trap: Int = -1   // 0 - not presented, 1 - on the place, -1 - unknown
  var ore: Int = -1    // -1 - unknown, or amount of ore
  var enemy: Boolean = false
  var me: Boolean = false
}

class GameData(var ore: List[(Int, Int)])

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

  def closest(p: (Int, Int), items: List[(Int, Int)]) = if (items.isEmpty) p else items.minBy(item => euclidean(p, item))
  def euclidean(a: (Int, Int), b: (Int, Int)) = sqrt(pow(b._1 - a._1, 2) + pow(b._2 - a._2, 2))

  def assign(robot: Robot, coord: (Int, Int)) = {
      robot.command = "DIG"
      robot.targetX = coord._1
      robot.targetY = coord._2
      robot.isFlying = true
//      coord
  }

  def setRandomCoord(boardData: Map[(Int, Int), Square], robot: Robot) {
      val coords = boardData.withFilter(entity => !entity._2.hole &&
                                                   entity._2.trap != 1 &&
                                                   entity._2.radar != 1).map(_._1).filter(c => c._1 != 0).toArray
//    val cl = closest((robot.x, robot.y), coords)
//    assign(robot, cl)

      val coordRand = coords(Random.nextInt(coords.length))
      assign(robot, coordRand)
  }

  def dig(boardData: Map[(Int, Int), Square],
          gameData: GameData,
          robot: Robot) = {

//    Console.err.println(s"-----------!!ORE!!-------------")
//    gameData.ore.foreach(Console.err.println)

    gameData.ore.headOption match {
      case Some(coord) =>
        assign(robot, coord)

//        gameData.mapDataMap = gameData.mapDataMap - assign(robot, coord)
        gameData.ore = gameData.ore.tail
      case None => setRandomCoord(boardData, robot)
    }
  }

  def enrichRobots(robot: Robot,
                   boardData: Map[(Int, Int), Square],
                   gameData: GameData,
                   radarcooldown: Int, trapcooldown: Int) {
//    Console.err.println(s"BEFORE COMMAND: ${robot.i}: (${robot.x},${robot.y}) ${robot.getCommand} / ${robot.isFlying}")
//    robot.isFlying = robot.targetX
    if (robot.targetX < 0 || robot.targetY < 0) {
      robot.command = "WAIT"
      Console.err.println(s"ROBOT ${robot.i} IS WRONG!!")
    } else {
      if (boardData((robot.targetX, robot.targetY)).hole &&
        (boardData((robot.targetX, robot.targetY)).ore == 0 || boardData((robot.targetX, robot.targetY)).ore == -1)) {
        robot.isFlying = false
      }

      if (robot.x == 0) {
        robot.isFlying = false
      }

      if (robot.item == 4) {
        robot.command = "MOVE"
        robot.targetX = 0
        robot.targetY = robot.y
      } else if (robot.item == 2) {
        if (!robot.isFlying) {
          dig(boardData, gameData, robot)
        }
      } else if (robot.item == 3) {
        if (!robot.isFlying) {
          boardData.find(sq => !sq._2.hole && sq._2.ore == 0) match {
            case Some(tp) => assign(robot, tp._1)
            case None => dig(boardData, gameData, robot)
          }
        }
      } else if (robot.item == -1) {
        if (!robot.isFlying) {
  //        Console.err.println(s"-1 SECTION :: BEFORE COMMAND: ${robot.i}: (${robot.x},${robot.y}) ${robot.getCommand} / ${robot.isFlying}")
  //        Console.err.println(s"trapRequested:: $trapRequested\tradarRequested::$radarRequested")
          if (!radarRequested && radarcooldown == 0) {
            robot.command = "REQUEST RADAR"
            radarRequested = true
          } else if (!trapRequested && trapcooldown == 0) {
            if (boardData.exists(sq => !sq._2.hole && sq._2.ore == 0)) {
                robot.command = "REQUEST TRAP"
                trapRequested = true
            } else dig(boardData, gameData, robot)

/*
            val trapsPlaces = boardData.withFilter(sq => !sq._2.hole && sq._2.ore == 0).map(_._1)
            Console.err.println(s"--------- TRAP PLACES -------------")
            trapsPlaces.foreach(tp => Console.err.print(s"$tp "))
            Console.err.println
*/
          } else {
            dig(boardData, gameData, robot)
          }
        }
      }
    }
//    Console.err.println(s"AFTER COMMAND: ${robot.i}: (${robot.x},${robot.y}) ${robot.getCommand} / ${robot.isFlying}")
  }

  def command(robots: List[Robot],
              boardData: Map[(Int, Int), Square],
              radarcooldown: Int, trapcooldown: Int) = {

    radarRequested = false
    trapRequested = false

    val oreMap = boardData.filter(_._2.ore > 0)
    val traps = boardData.withFilter(_._2.trap == 1).map(_._1).toSet

    val ore = oreMap.map(kv => List.fill(kv._2.ore)(kv._1)).flatten.filterNot(traps).toList
    val gameData = new GameData(ore)

    Console.err.println(s"--------- TRAPS -------------")
    traps.foreach(trap => Console.err.print(s"$trap "))
    Console.err.println
/*
    Console.err.println(s"--------- NOTH -------------")
    trapsPlaces.foreach(noth => Console.err.print(s"$noth "))
    Console.err.println
    Console.err.println(s"${oreMap.keys}")

    Console.err.println(s"----------------- ROBOTS ------------------------------------------------")
    robots.foreach(_.print)
    Console.err.println(s"----------------- ENTITY DATA -------------------------------------------")
    entityData.foreach(ed => {
      Console.err.println(s"[${ed(0)}: ${ed(1)}, (${ed(2)}, ${ed(3)}): ${ed(4)}], ")
    })
    Console.err.println(s"----------------- MAP DATA MAP -------------------------------------------")
    mapDataMap.foreach(Console.err.println)
    Console.err.println(s"(11,12) ${mapDataMap(11, 12)}")
    Console.err.println(s"(12,8) ${mapDataMap(12, 8)}")
    Console.err.println(s"----------------- noQuestionMark -------------------------------------------")

    noQuestionMark.foreach(pair => Console.err.println(s"noQuestionMark: $pair"))
*/

//    val noHoles = mapDataMap.filter(_._2._2 != 0)
//    Console.err.println(s"noHoles=${noHoles.size}")

    robots.foreach(r => {
      enrichRobots(r, boardData, gameData, radarcooldown, trapcooldown)
    })
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

  def createBoardData(width: Int, height: Int) = {
    (for (i <- 0 until height; j <- 0 until width) yield (j,i) -> new Square(j,i)).toMap
  }
  def enrichBoardData(mapData: List[Array[(String, Int)]], boardData: Map[(Int, Int), Square]) {
    for (i <- mapData.indices; j <- mapData(i).indices) {
      val square = boardData((j, i))
      val (ore, hole) = mapData(i)(j)
      if (ore != "?") square.ore = ore.toInt
      square.hole = hole == 1
    }
  }

  var firstMove = true

  // height: size of the map
  val Array(width, height) = for (i <- readLine split " ") yield i.toInt
  Console.err.println(s"width=$width\theight=$height")

  var myRobotsRaw: List[Robot] = List.empty
  var myRobotsMap: Map[Int, Robot] = Map.empty
  var boardData = Map.empty[(Int, Int), Square]

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
      boardData = createBoardData(width, height)
    } else {
      enrichBoardData(mapData, boardData)
      entityData.foreach(dataset => {
        if (dataset(2) > -1 && dataset(3) > -1) {
          val square = boardData((dataset(2), dataset(3)))
          dataset(1) match {
            case 0 => square.me = true
            case 1 => square.enemy = true
            case 2 => square.radar = 1
            case 3 => square.trap = 1
          }
        }
      })
      myRobotsRaw.foreach(robot => {
        val data = myRobotsDataMap(robot.i)
        robot.x = data(2)
        robot.y = data(3)
        robot.item = data(4)
      })
    }

    val myRobots = RobotManager.command(myRobotsRaw, boardData, radarcooldown, trapcooldown)


    if (myRobots.length < 5) Console.err.println(s"ROBOT IS DEAD!")

    for (i <- 0 until 5) {
      // Write an action using println
      // To debug: Console.err.println("Debug messages...")

      println(myRobotsMap(i).getCommand) // WAIT|MOVE x y|DIG x y|REQUEST item
    }

    firstMove = false
  }
}