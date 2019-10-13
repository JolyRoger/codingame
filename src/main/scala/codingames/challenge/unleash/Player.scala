//package codingames.challenge.unleash

import math._

/*
case class Square2(x: Int, y: Int) {
  lazy val getX = x
  lazy val getY = y
  var hole: Boolean = false
  var myHole: Boolean = false
  var suspect = false
  var radar: Int = -1  // 0 - not presented, 1 - on the place, -1 - unknown
  var trap: Int = -1   // 0 - not presented, 1 - on the place, -1 - unknown
  var ore: Int = -1    // -1 - unknown, or amount of ore
  var enemy: Boolean = false
  var me: Boolean = false
  var radarAffected: Set[(Int, Int)] = Set.empty
  def senseless = ore == 0 || trap == 1 || dangerous
  def dangerous = (hole && !myHole) || suspect
  def radarAffectedAmount(boardData: Map[(Int, Int), Square]) = radarAffected.count(boardData(_).ore == -1)
}
*/

class Square(x: Int, y: Int) {
  lazy val getX = x
  lazy val getY = y
  var hole: Boolean = false
  var myHole: Boolean = false
  var suspect = false
  var radar: Int = -1  // 0 - not presented, 1 - on the place, -1 - unknown
  var trap: Int = -1   // 0 - not presented, 1 - on the place, -1 - unknown
  var ore: Int = -1    // -1 - unknown, or amount of ore
  var enemy: Boolean = false
  var me: Boolean = false
  var radarAffected: Set[(Int, Int)] = Set.empty
  def senseless = ore == 0 || trap == 1 || dangerous
  def dangerous = (hole && !myHole) || suspect
  def radarAffectedAmount(boardData: Map[(Int, Int), Square]) = radarAffected.count(boardData(_).ore == -1)
}
class RadarOrdering(boardData: Map[(Int, Int), Square]) extends Ordering[Square] {
  def compare(a: Square, b: Square) = {
    val ar = a.radarAffectedAmount(boardData)
    val br = b.radarAffectedAmount(boardData)
    if (ar > br) 1 else if (ar < br) -1 else 0
  }
}
object GameData {
  var ore: Set[(Int, Int)] = Set.empty
}

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
//  def print = Console.err.println(s"$i: ($x, $y) - $item")
}

object RobotManager {

  var radarRequested = false
  var trapRequested = false

  def closest(p: (Int, Int), items: Set[(Int, Int)]) = if (items.isEmpty) p else items.minBy(item => euclidean(p, item))
  def euclidean(a: (Int, Int), b: (Int, Int)) = sqrt(pow(b._1 - a._1, 2) + pow(b._2 - a._2, 2))

  def assign(robot: Robot, coord: (Int, Int)) = {
      robot.command = "DIG"
      robot.targetX = coord._1
      robot.targetY = coord._2
      robot.isFlying = true
//      coord
  }

  def dig(boardData: Map[(Int, Int), Square],
          robot: Robot) = {
    if (GameData.ore.nonEmpty) {
      val closestOre = closest((robot.x, robot.y), GameData.ore)
      assign(robot, closestOre)
      GameData.ore = GameData.ore - closestOre
    } else {
      val coords = boardData.filter(square => !square._2.hole &&
                                               square._2.radar != 1 &&
                                               square._1._1 != 0 &&
                                               square._2.ore == -1).keySet
      val closestOre = closest((robot.x, robot.y), coords)
      assign(robot, closestOre)
    }
  }

  def enrichRobots(robot: Robot,
                   boardData: Map[(Int, Int), Square],
                   radarcooldown: Int, trapcooldown: Int) {
    // if (robot.i == 8 || robot.i == 9) Console.err.println(s"BEFORE COMMAND: ${robot.i}: (${robot.x},${robot.y}) ${robot.getCommand} / ${robot.isFlying} item=${robot.item}")
    
//    robot.isFlying = robot.targetX
    if (robot.x < 0 || robot.y < 0 || robot.targetX < 0 || robot.targetY < 0) {
      robot.command = "WAIT"
//      Console.err.println(s"ROBOT ${robot.i} IS DEAD :(")
    } else {
      val targetSquare = boardData((robot.targetX, robot.targetY))
      if (targetSquare.hole && robot.item != 2 && (targetSquare.ore == 0 || targetSquare.ore == -1)) {                                     // FIXME ???
//            Console.err.println(s"${robot.i}> first: ${targetSquare.hole} second: ${targetSquare.ore == 0} third: ${targetSquare.ore == -1}")
//            if (robot.i == 8) Console.err.println(s"isFlying false")
        robot.isFlying = false
      }

      if (robot.x == 0) {
        robot.isFlying = false
      }

      if (robot.item == 4) {
        robot.command = "MOVE"
        robot.targetX = 0
        robot.targetY = robot.y
        robot.isFlying = true
      } else if (robot.item == 2) {   // radar
        if (!robot.isFlying) {
//          val radarOrdering = new RadarOrdering(boardData)
          val radarAffectedAmountMap = boardData.filter(square => square._1._1 != 0 && square._2.trap != 1 && !square._2.dangerous).groupBy(sq => sq._2.radarAffectedAmount(boardData))
//          val maxSquare = squares.max(radarOrdering)

//          Console.err.println(s"RADAR maxSquare.size=${maxSquare.size} maxSquare.xy=[${maxSquare.head.getX},${maxSquare.head.getY}]")
//            .max(new RadarOrdering(boardData))
          assign(robot, closest((robot.x, robot.y), radarAffectedAmountMap(radarAffectedAmountMap.keySet.max).values.map(square => (square.getX, square.getY)).toSet))
        }
      } else if (robot.item == 3) {   // trap
          if (!robot.isFlying || targetSquare.senseless) {
            boardData.find(sq => sq._2.ore == 2 && sq._2.trap != 1 && !sq._2.dangerous) match {
              case Some(tp) => assign(robot, tp._1)
              case None => dig(boardData, robot)
            }
          }
      } else if (robot.item == -1) {
        if (!robot.isFlying) {
  //        Console.err.println(s"-1 SECTION :: BEFORE COMMAND: ${robot.i}: (${robot.x},${robot.y}) ${robot.getCommand} / ${robot.isFlying}")
  //        Console.err.println(s"trapRequested:: $trapRequested\tradarRequested::$radarRequested")
          if (!radarRequested && radarcooldown == 0 && robot.x < 13) {
            robot.command = "REQUEST RADAR"
            radarRequested = true
          } else if (!trapRequested && trapcooldown == 0 && robot.x < 4) {
            if (boardData.exists(_._2.ore > 2)) {
                robot.command = "REQUEST TRAP"
                trapRequested = true
            } else dig(boardData, robot)

//            val trapsPlaces = boardData.withFilter(sq => !sq._2.hole && sq._2.ore == 0).map(_._1)
//            Console.err.println(s"--------- TRAP PLACES -------------")
//            trapsPlaces.foreach(tp => Console.err.print(s"$tp "))
//            Console.err.println

          } else {
            dig(boardData, robot)
          } 
            
          
        } else {
//            Console.err.print(s"${robot.i}\t")
//            Console.err.println(s"targetSquare:(${targetSquare.getX}, ${targetSquare.getY}) ore:${targetSquare.ore} danger:${targetSquare.dangerous}")
            if (targetSquare.senseless) {
                robot.isFlying = false
                dig(boardData, robot)
            }
        }
      }
    }
//    Console.err.println(s"AFTER COMMAND: ${robot.i}: (${robot.x},${robot.y}) ${robot.getCommand} / ${robot.isFlying}")
  }

  def command(robots: List[Robot],
              boardData: Map[(Int, Int), Square],
              radarcooldown: Int, trapcooldown: Int) {

    val (traps, safe) = boardData.filter(_._2.ore > 0).partition(sq => sq._2.trap == 1 || sq._2.dangerous)
//    Console.err.println(s"${traps.size} / ${safe.size}")
    val ore = safe.map(kv => List.fill(kv._2.ore)(kv._1)).flatten.toSet
    GameData.ore = ore

    radarRequested = robots.size <= 2 || ore.size > robots.size
    trapRequested = robots.size <= 2 || traps.size >= 7

/*
    Console.err.println(s"--------- MY HOLES -------------")
    val (dang, safe) = boardData.filter(_._2.hole).partition(_._2.dangerous)
    Console.err.print(s"DANG: ")
    dang.keySet.foreach(d => Console.err.print(s"$d "))
    Console.err.println
    Console.err.print(s"SAFE: ")
    safe.keySet.foreach(s => Console.err.print(s"$s "))
    Console.err.println
    Console.err.println(s"--------- TRAPS -------------")
    traps.foreach(trap => Console.err.print(s"$trap "))
    Console.err.println
    Console.err.println(s"radarRequested=$radarRequested trapRequested=$trapRequested")
    Console.err.println(s"--------- ORE -------------")
    ore.foreach(visOre => Console.err.print(s"$visOre "))
    Console.err.println
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
    val noHoles = mapDataMap.filter(_._2._2 != 0)
    Console.err.println(s"noHoles=${noHoles.size}")
*/


    robots.foreach(enrichRobots(_, boardData, radarcooldown, trapcooldown))
  }
}

/**
 * Deliver more ore to hq (left side of the map) than your opponent. Use radars to find ore but beware of traps!
 **/
object Player extends App {
  def adjacent(square: (Int, Int)) = Set((square._1, square._2),
    (if (square._1 > 0) square._1 - 1 else square._1, square._2),
    (if (square._1 < 29) square._1 + 1 else square._1, square._2),
    (square._1, if (square._2 > 0) square._2 - 1 else square._2),
    (square._1, if (square._2 < 14) square._2 + 1 else square._2))

  def oreHole(inputs: Array[String]) = {
    val res = inputs.zipWithIndex.partition(in => in._2 % 2 == 0)
    res._1.map(_._1).zip(res._2.map(_._1.toInt))/*.zipWithIndex*/
  }

  def calc(coord: (Int, Int), offset: Int, leftLimit: Int, rightLimit: Int) = {
    val (x,y) = coord
    val internalOffset = 4 - Math.abs(offset)
    val a = (x - internalOffset to x).dropWhile(_ < leftLimit).toSet
    val b = (x to x + internalOffset).takeWhile(_ <= rightLimit).toSet
    (a ++ Set(x) ++ b).map((_, y - offset)).filter(pair => pair._2 >= 0 && pair._2 < 15)
  }

  def calcSquare(coord: (Int, Int)) = {
    (for (offset <- -4 to 4) yield calc(coord, offset, 1, 29)).toSet.flatten
  }

  def calcVisibleSquares(boardData: Map[(Int, Int), Square]) {
    boardData.foreach(pair => {
      pair._2.radarAffected = calcSquare(pair._1)
    })
  }

  def createBoardData(width: Int, height: Int) = {
    (for (i <- 0 until height; j <- 0 until width) yield (j,i) -> new Square(j,i)).toMap
  }
  def enrichBoardData(mapData: List[Array[(String, Int)]], boardData: Map[(Int, Int), Square]) {
    for (i <- mapData.indices; j <- mapData(i).indices) {
      val square = boardData((j, i))
      val (oreStr, hole) = mapData(i)(j)
      if (oreStr != "?") {
        val ore = oreStr.toInt
        if (square.ore != ore && square.ore > -1) {
//          Console.err.println(s"Square (${square.getX},${square.getY}) is suspect! Ore is $ore. Expected is ${square.ore}")
          square.suspect = true
        }
        square.ore = ore
      } else square.ore = -1
      square.hole = hole == 1
    }
  }

  def kamikaze(robot: Robot, enemyRobotsData: List[(Int, Int)]) {
    val adj: List[(Int, Int)] = adjacent((robot.x, robot.y)).toList

    adj.foreach(coord => {
      val square = boardData(coord)
      if ((square.suspect && square.ore > 0) || square.trap == 1) {
        val coordAdj = adjacent(coord).toList
        val myRobotsCoord = myRobotList.map(r => (r.x, r.y))
        val enemyRobotsNear = coordAdj.intersect(enemyRobotsData)
        val myRobotsNear = coordAdj.intersect(myRobotsCoord)

        if (myRobotsNear.size < enemyRobotsNear.size) {
          robot.command = "DIG"
          robot.targetX = coord._1
          robot.targetY = coord._2
        }
      }
    })
  }

  var firstMove = true

  // height: size of the map
  val Array(width, height) = for (i <- readLine split " ") yield i.toInt
//  Console.err.println(s"width=$width\theight=$height")

  var myRobotList: List[Robot] = List.empty
  var myRobotsMap: Map[Int, Robot] = Map.empty
  var boardData = Map.empty[(Int, Int), Square]

  // game loop
  while (true) {
    // myscore: Amount of ore delivered
    /*val Array(myscore, opponentscore) = */for (i <- readLine split " ") {} /*yield i.toInt*/
//    Console.err.println(s"myscore=$myscore\topponentscore=$opponentscore")

    val mapData = (for (i <- 0 until height) yield
//      Console.err.println(s"inputsStr=$inputsStr")
      oreHole(readLine split " ")
/*
      for (j <- 0 until width) {
        // ore: amount of ore or "?" if unknown
        // hole: 1 if cell has a hole
        val hole = inputs(2 * j + 1).toInt
        Console.err.print(s"$hole ")
      }
      Console.err.println
*/).toList

//    Console.err.println("----------------------------------")

    // entitycount: number of entities visible to you
    // radarcooldown: turns left until a new radar can be requested
    // trapcooldown: turns left until a new trap can be requested
    val Array(entitycount, radarcooldown, trapcooldown) = for (i <- readLine split " ") yield i.toInt
//    Console.err.println(s"entitycount=$entitycount radarcooldown=$radarcooldown trapcooldown=$trapcooldown")

    val entityData = (for (i <- 0 until entitycount) yield
      // id: unique id of the entity
      // sort: 0 for your robot, 1 for other robot, 2 for radar, 3 for trap
      // y: position of the entity
      // item: if this entity is a robot, the item it is carrying (-1 for NONE, 2 for RADAR, 3 for TRAP, 4 for ORE)
      /*val Array(id, sort, x, y, item) = */for (i <- readLine split " ") yield i.toInt).toList
//      Console.err.println(s"id=$id sort=$sort x=$x y=$y item=$item")


//    val myRobotsDataMap = entityData.withFilter(_(1) == 0).map(data => data(0) -> data).toMap

    val dataMap = entityData.groupBy(_(1))
    val myRobotsDataMap = dataMap(0).map(data => data(0) -> data).toMap
    val enemyRobotsData = dataMap(1).map(data => (data(2), data(3)))
//    val myRobotsDataMap = entityData.withFilter(_(1) == 0).map(data => data(0) -> data).toMap

    if (firstMove) {
      myRobotsMap = myRobotsDataMap.keys.map(key => {
        val dataset = myRobotsDataMap(key)
        key -> new Robot(dataset(0), dataset(2), dataset(3), dataset(4))
      }).toMap
//      myRobotsRaw = myRobotsData.map(dataset => new Robot(dataset(0), dataset(2), dataset(3), dataset(4)))
      myRobotList = myRobotsMap.values.toList.sortBy(_.i)
//      myRobotsMap = myRobotsRaw.map(robot => robot.i -> robot).toMap
      boardData = createBoardData(width, height)
      calcVisibleSquares(boardData)
      firstMove = false
    }

    enrichBoardData(mapData, boardData)
    entityData.foreach(dataset => {
      if (dataset(2) > -1 && dataset(3) > -1) {
        val square = boardData((dataset(2), dataset(3)))
//          if (square.getX == 20 && square.getY == 8) {
//            Console.err.println(s"(20,8) trap=${square.trap} data=${dataset(4)}")
//          }
        dataset(1) match {
          case 0 => square.me = true
          case 1 => square.enemy = true
          case 2 => square.radar = 1
          case 3 => square.trap = 1
        }
      }
    })
    myRobotList.foreach(robot => {
      val data = myRobotsDataMap(robot.i)
      robot.x = data(2)
      robot.y = data(3)
      robot.item = data(4)
    })

    RobotManager.command(myRobotList, boardData, radarcooldown, trapcooldown)

//    for (i <- 0 until 5) {
      // Write an action using println
      // To debug: Console.err.println("Debug messages...")
//      val sortedRobots = myRobotsRaw

      myRobotList.foreach(robot => {

    //   Console.err.print(s"robot ${robot.i} isFlying:${robot.isFlying} ")
    //   Console.err.print(s"target: (${robot.targetX}, ${robot.targetY} ore: ${boardData((robot.targetX, robot.targetY)).ore} ")
    //   Console.err.print(s"hole: ${boardData((robot.targetX, robot.targetY)).hole} ")
    //   Console.err.println(s"mapDataYX:${mapData(robot.targetY)(robot.targetX)}")

//        Console.err.println(s"${robot.i} > ${robot.getCommand} ${(robot.x, robot.y)}/${(robot.targetX, robot.targetY)}  near:${robot.near} danger:${boardData((robot.targetX, robot.targetY)).dangerous}")

//         Console.err.println(s"${robot.i}(${robot.x}, ${robot.y}) > :${robot.getCommand} flying:${robot.isFlying}")

//        kamikaze(robot, enemyRobotsData)
        println(robot.getCommand) // WAIT|MOVE x y|DIG x y|REQUEST item

       if (robot.command == "DIG") {
//           Console.err.println(s"!!!SET MY HOLE ${robot.i} > ${(robot.targetY, robot.targetY)}")
         val square = boardData((robot.targetX, robot.targetY))
         square.myHole = true
         square.ore = if (square.ore > 0) square.ore - 1 else square.ore
         robot.isFlying = false
       }

      })
//    }

  }
}