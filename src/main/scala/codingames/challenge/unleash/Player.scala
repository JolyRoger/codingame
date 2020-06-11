package codingames.challenge.unleash

import math._
import scala.io.StdIn._

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
  def radarAffectedAmount(boardData: Map[(Int, Int), Square2]) = radarAffected.count(boardData(_).ore == -1)
}

class RadarOrdering(boardData: Map[(Int, Int), Square2]) extends Ordering[Square2] {
  def compare(a: Square2, b: Square2) = {
    val ar = a.radarAffectedAmount(boardData)
    val br = b.radarAffectedAmount(boardData)
    if (ar > br) 1 else if (ar < br) -1 else 0
  }
}
class GameData(var ore: Set[(Int, Int)])

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

  def setClosestCoord(boardData2: Map[(Int, Int), Square2], robot: Robot) {
      val coords = boardData2.filter(square => !square._2.hole &&
                                               square._2.radar != 1 &&
                                               square._1._1 != 0 &&
                                               square._2.ore == -1).keySet
      val closestOre = closest((robot.x, robot.y), coords)
      assign(robot, closestOre)
  }

  def dig(boardData2: Map[(Int, Int), Square2],
          gameData: GameData,
          robot: Robot) = {

// Console.err.println(s"-----------!!ORE!!-------------")
//    gameData.ore.foreach(Console.err.println)
    if (gameData.ore.nonEmpty) {
      val closestOre = closest((robot.x, robot.y), gameData.ore)
      assign(robot, closestOre)
      gameData.ore = gameData.ore - closestOre
    } else {
      setClosestCoord(boardData2, robot)
    }
  }

  def enrichRobots(robot: Robot,
                   boardData2: Map[(Int, Int), Square2],
                   gameData: GameData,
                   radarcooldown: Int, trapcooldown: Int) {
    // if (robot.i == 8 || robot.i == 9) Console.err.println(s"BEFORE COMMAND: ${robot.i}: (${robot.x},${robot.y}) ${robot.getCommand} / ${robot.isFlying} item=${robot.item}")

//    robot.isFlying = robot.targetX
    if (robot.x < 0 || robot.y < 0 || robot.targetX < 0 || robot.targetY < 0) {
      robot.command = "WAIT"
//      Console.err.println(s"ROBOT ${robot.i} IS DEAD :(")
    } else {
      val targetSquare = boardData2((robot.targetX, robot.targetY))
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
          val radarAffectedAmountMap = boardData2.filter(square => square._1._1 != 0 && square._2.trap != 1 && !square._2.dangerous).groupBy(sq => sq._2.radarAffectedAmount(boardData2))
//          val maxSquare = squares.max(radarOrdering)

//          Console.err.println(s"RADAR maxSquare.size=${maxSquare.size} maxSquare.xy=[${maxSquare.head.getX},${maxSquare.head.getY}]")
//            .max(new RadarOrdering(boardData))
          assign(robot, closest((robot.x, robot.y), radarAffectedAmountMap(radarAffectedAmountMap.keySet.max).values.map(square => (square.getX, square.getY)).toSet))
        }
      } else if (robot.item == 3) {   // trap
          if (!robot.isFlying || targetSquare.senseless) {
            boardData2.find(sq => sq._2.ore == 2 && sq._2.trap != 1 && !sq._2.dangerous) match {
              case Some(tp) => assign(robot, tp._1)
              case None => dig(boardData2, gameData, robot)
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
            if (boardData2.exists(_._2.ore > 2)) {
                robot.command = "REQUEST TRAP"
                trapRequested = true
            } else dig(boardData2, gameData, robot)

//            val trapsPlaces = boardData.withFilter(sq => !sq._2.hole && sq._2.ore == 0).map(_._1)
//            Console.err.println(s"--------- TRAP PLACES -------------")
//            trapsPlaces.foreach(tp => Console.err.print(s"$tp "))
//            Console.err.println

          } else {
            dig(boardData2, gameData, robot)
          }


        } else {
//            Console.err.print(s"${robot.i}\t")
//            Console.err.println(s"targetSquare:(${targetSquare.getX}, ${targetSquare.getY}) ore:${targetSquare.ore} danger:${targetSquare.dangerous}")
            if (targetSquare.senseless) {
                robot.isFlying = false
                dig(boardData2, gameData, robot)
            }
        }
      }
    }
//    Console.err.println(s"AFTER COMMAND: ${robot.i}: (${robot.x},${robot.y}) ${robot.getCommand} / ${robot.isFlying}")
  }

  def command(robots: List[Robot],
              boardData2: Map[(Int, Int), Square2],
              radarcooldown: Int, trapcooldown: Int,
              brave: Boolean) {

    val (traps, safe) = boardData2.filter(_._2.ore > 0).partition(sq => sq._2.trap == 1 || (sq._2.dangerous && !brave))
//    Console.err.println(s"${traps.size} / ${safe.size}")
    val ore = safe.map(kv => List.fill(kv._2.ore)(kv._1)).flatten.toSet
    val gameData = new GameData(ore)

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


    robots.foreach(enrichRobots(_, boardData2, gameData, radarcooldown, trapcooldown))
  }
}

/**
 * Deliver more ore to hq (left side of the map) than your opponent. Use radars to find ore but beware of traps!
 **/
object Player extends App {
  val boardData2: Map[(Int, Int), Square2] = Map(
    (0,0) -> Square2(0,0), (1,0) -> Square2(1,0), (2,0) -> Square2(2,0), (3,0) -> Square2(3,0), (4,0) -> Square2(4,0), (5,0) -> Square2(5,0), (6,0) -> Square2(6,0), (7,0) -> Square2(7,0), (8,0) -> Square2(8,0), (9,0) -> Square2(9,0), (10,0) -> Square2(10,0), (11,0) -> Square2(11,0), (12,0) -> Square2(12,0), (13,0) -> Square2(13,0), (14,0) -> Square2(14,0), (15,0) -> Square2(15,0), (16,0) -> Square2(16,0), (17,0) -> Square2(17,0), (18,0) -> Square2(18,0), (19,0) -> Square2(19,0), (20,0) -> Square2(20,0), (21,0) -> Square2(21,0), (22,0) -> Square2(22,0), (23,0) -> Square2(23,0), (24,0) -> Square2(24,0), (25,0) -> Square2(25,0), (26,0) -> Square2(26,0), (27,0) -> Square2(27,0), (28,0) -> Square2(28,0), (29,0) -> Square2(29,0),
    (0,1) -> Square2(0,1), (1,1) -> Square2(1,1), (2,1) -> Square2(2,1), (3,1) -> Square2(3,1), (4,1) -> Square2(4,1), (5,1) -> Square2(5,1), (6,1) -> Square2(6,1), (7,1) -> Square2(7,1), (8,1) -> Square2(8,1), (9,1) -> Square2(9,1), (10,1) -> Square2(10,1), (11,1) -> Square2(11,1), (12,1) -> Square2(12,1), (13,1) -> Square2(13,1), (14,1) -> Square2(14,1), (15,1) -> Square2(15,1), (16,1) -> Square2(16,1), (17,1) -> Square2(17,1), (18,1) -> Square2(18,1), (19,1) -> Square2(19,1), (20,1) -> Square2(20,1), (21,1) -> Square2(21,1), (22,1) -> Square2(22,1), (23,1) -> Square2(23,1), (24,1) -> Square2(24,1), (25,1) -> Square2(25,1), (26,1) -> Square2(26,1), (27,1) -> Square2(27,1), (28,1) -> Square2(28,1), (29,1) -> Square2(29,1),
    (0,2) -> Square2(0,2), (1,2) -> Square2(1,2), (2,2) -> Square2(2,2), (3,2) -> Square2(3,2), (4,2) -> Square2(4,2), (5,2) -> Square2(5,2), (6,2) -> Square2(6,2), (7,2) -> Square2(7,2), (8,2) -> Square2(8,2), (9,2) -> Square2(9,2), (10,2) -> Square2(10,2), (11,2) -> Square2(11,2), (12,2) -> Square2(12,2), (13,2) -> Square2(13,2), (14,2) -> Square2(14,2), (15,2) -> Square2(15,2), (16,2) -> Square2(16,2), (17,2) -> Square2(17,2), (18,2) -> Square2(18,2), (19,2) -> Square2(19,2), (20,2) -> Square2(20,2), (21,2) -> Square2(21,2), (22,2) -> Square2(22,2), (23,2) -> Square2(23,2), (24,2) -> Square2(24,2), (25,2) -> Square2(25,2), (26,2) -> Square2(26,2), (27,2) -> Square2(27,2), (28,2) -> Square2(28,2), (29,2) -> Square2(29,2),
    (0,3) -> Square2(0,3), (1,3) -> Square2(1,3), (2,3) -> Square2(2,3), (3,3) -> Square2(3,3), (4,3) -> Square2(4,3), (5,3) -> Square2(5,3), (6,3) -> Square2(6,3), (7,3) -> Square2(7,3), (8,3) -> Square2(8,3), (9,3) -> Square2(9,3), (10,3) -> Square2(10,3), (11,3) -> Square2(11,3), (12,3) -> Square2(12,3), (13,3) -> Square2(13,3), (14,3) -> Square2(14,3), (15,3) -> Square2(15,3), (16,3) -> Square2(16,3), (17,3) -> Square2(17,3), (18,3) -> Square2(18,3), (19,3) -> Square2(19,3), (20,3) -> Square2(20,3), (21,3) -> Square2(21,3), (22,3) -> Square2(22,3), (23,3) -> Square2(23,3), (24,3) -> Square2(24,3), (25,3) -> Square2(25,3), (26,3) -> Square2(26,3), (27,3) -> Square2(27,3), (28,3) -> Square2(28,3), (29,3) -> Square2(29,3),
    (0,4) -> Square2(0,4), (1,4) -> Square2(1,4), (2,4) -> Square2(2,4), (3,4) -> Square2(3,4), (4,4) -> Square2(4,4), (5,4) -> Square2(5,4), (6,4) -> Square2(6,4), (7,4) -> Square2(7,4), (8,4) -> Square2(8,4), (9,4) -> Square2(9,4), (10,4) -> Square2(10,4), (11,4) -> Square2(11,4), (12,4) -> Square2(12,4), (13,4) -> Square2(13,4), (14,4) -> Square2(14,4), (15,4) -> Square2(15,4), (16,4) -> Square2(16,4), (17,4) -> Square2(17,4), (18,4) -> Square2(18,4), (19,4) -> Square2(19,4), (20,4) -> Square2(20,4), (21,4) -> Square2(21,4), (22,4) -> Square2(22,4), (23,4) -> Square2(23,4), (24,4) -> Square2(24,4), (25,4) -> Square2(25,4), (26,4) -> Square2(26,4), (27,4) -> Square2(27,4), (28,4) -> Square2(28,4), (29,4) -> Square2(29,4),
    (0,5) -> Square2(0,5), (1,5) -> Square2(1,5), (2,5) -> Square2(2,5), (3,5) -> Square2(3,5), (4,5) -> Square2(4,5), (5,5) -> Square2(5,5), (6,5) -> Square2(6,5), (7,5) -> Square2(7,5), (8,5) -> Square2(8,5), (9,5) -> Square2(9,5), (10,5) -> Square2(10,5), (11,5) -> Square2(11,5), (12,5) -> Square2(12,5), (13,5) -> Square2(13,5), (14,5) -> Square2(14,5), (15,5) -> Square2(15,5), (16,5) -> Square2(16,5), (17,5) -> Square2(17,5), (18,5) -> Square2(18,5), (19,5) -> Square2(19,5), (20,5) -> Square2(20,5), (21,5) -> Square2(21,5), (22,5) -> Square2(22,5), (23,5) -> Square2(23,5), (24,5) -> Square2(24,5), (25,5) -> Square2(25,5), (26,5) -> Square2(26,5), (27,5) -> Square2(27,5), (28,5) -> Square2(28,5), (29,5) -> Square2(29,5),
    (0,6) -> Square2(0,6), (1,6) -> Square2(1,6), (2,6) -> Square2(2,6), (3,6) -> Square2(3,6), (4,6) -> Square2(4,6), (5,6) -> Square2(5,6), (6,6) -> Square2(6,6), (7,6) -> Square2(7,6), (8,6) -> Square2(8,6), (9,6) -> Square2(9,6), (10,6) -> Square2(10,6), (11,6) -> Square2(11,6), (12,6) -> Square2(12,6), (13,6) -> Square2(13,6), (14,6) -> Square2(14,6), (15,6) -> Square2(15,6), (16,6) -> Square2(16,6), (17,6) -> Square2(17,6), (18,6) -> Square2(18,6), (19,6) -> Square2(19,6), (20,6) -> Square2(20,6), (21,6) -> Square2(21,6), (22,6) -> Square2(22,6), (23,6) -> Square2(23,6), (24,6) -> Square2(24,6), (25,6) -> Square2(25,6), (26,6) -> Square2(26,6), (27,6) -> Square2(27,6), (28,6) -> Square2(28,6), (29,6) -> Square2(29,6),
    (0,7) -> Square2(0,7), (1,7) -> Square2(1,7), (2,7) -> Square2(2,7), (3,7) -> Square2(3,7), (4,7) -> Square2(4,7), (5,7) -> Square2(5,7), (6,7) -> Square2(6,7), (7,7) -> Square2(7,7), (8,7) -> Square2(8,7), (9,7) -> Square2(9,7), (10,7) -> Square2(10,7), (11,7) -> Square2(11,7), (12,7) -> Square2(12,7), (13,7) -> Square2(13,7), (14,7) -> Square2(14,7), (15,7) -> Square2(15,7), (16,7) -> Square2(16,7), (17,7) -> Square2(17,7), (18,7) -> Square2(18,7), (19,7) -> Square2(19,7), (20,7) -> Square2(20,7), (21,7) -> Square2(21,7), (22,7) -> Square2(22,7), (23,7) -> Square2(23,7), (24,7) -> Square2(24,7), (25,7) -> Square2(25,7), (26,7) -> Square2(26,7), (27,7) -> Square2(27,7), (28,7) -> Square2(28,7), (29,7) -> Square2(29,7),
    (0,8) -> Square2(0,8), (1,8) -> Square2(1,8), (2,8) -> Square2(2,8), (3,8) -> Square2(3,8), (4,8) -> Square2(4,8), (5,8) -> Square2(5,8), (6,8) -> Square2(6,8), (7,8) -> Square2(7,8), (8,8) -> Square2(8,8), (9,8) -> Square2(9,8), (10,8) -> Square2(10,8), (11,8) -> Square2(11,8), (12,8) -> Square2(12,8), (13,8) -> Square2(13,8), (14,8) -> Square2(14,8), (15,8) -> Square2(15,8), (16,8) -> Square2(16,8), (17,8) -> Square2(17,8), (18,8) -> Square2(18,8), (19,8) -> Square2(19,8), (20,8) -> Square2(20,8), (21,8) -> Square2(21,8), (22,8) -> Square2(22,8), (23,8) -> Square2(23,8), (24,8) -> Square2(24,8), (25,8) -> Square2(25,8), (26,8) -> Square2(26,8), (27,8) -> Square2(27,8), (28,8) -> Square2(28,8), (29,8) -> Square2(29,8),
    (0,9) -> Square2(0,9), (1,9) -> Square2(1,9), (2,9) -> Square2(2,9), (3,9) -> Square2(3,9), (4,9) -> Square2(4,9), (5,9) -> Square2(5,9), (6,9) -> Square2(6,9), (7,9) -> Square2(7,9), (8,9) -> Square2(8,9), (9,9) -> Square2(9,9), (10,9) -> Square2(10,9), (11,9) -> Square2(11,9), (12,9) -> Square2(12,9), (13,9) -> Square2(13,9), (14,9) -> Square2(14,9), (15,9) -> Square2(15,9), (16,9) -> Square2(16,9), (17,9) -> Square2(17,9), (18,9) -> Square2(18,9), (19,9) -> Square2(19,9), (20,9) -> Square2(20,9), (21,9) -> Square2(21,9), (22,9) -> Square2(22,9), (23,9) -> Square2(23,9), (24,9) -> Square2(24,9), (25,9) -> Square2(25,9), (26,9) -> Square2(26,9), (27,9) -> Square2(27,9), (28,9) -> Square2(28,9), (29,9) -> Square2(29,9),
    (0,10) -> Square2(0,10), (1,10) -> Square2(1,10), (2,10) -> Square2(2,10), (3,10) -> Square2(3,10), (4,10) -> Square2(4,10), (5,10) -> Square2(5,10), (6,10) -> Square2(6,10), (7,10) -> Square2(7,10), (8,10) -> Square2(8,10), (9,10) -> Square2(9,10), (10,10) -> Square2(10,10), (11,10) -> Square2(11,10), (12,10) -> Square2(12,10), (13,10) -> Square2(13,10), (14,10) -> Square2(14,10), (15,10) -> Square2(15,10), (16,10) -> Square2(16,10), (17,10) -> Square2(17,10), (18,10) -> Square2(18,10), (19,10) -> Square2(19,10), (20,10) -> Square2(20,10), (21,10) -> Square2(21,10), (22,10) -> Square2(22,10), (23,10) -> Square2(23,10), (24,10) -> Square2(24,10), (25,10) -> Square2(25,10), (26,10) -> Square2(26,10), (27,10) -> Square2(27,10), (28,10) -> Square2(28,10), (29,10) -> Square2(29,10),
    (0,11) -> Square2(0,11), (1,11) -> Square2(1,11), (2,11) -> Square2(2,11), (3,11) -> Square2(3,11), (4,11) -> Square2(4,11), (5,11) -> Square2(5,11), (6,11) -> Square2(6,11), (7,11) -> Square2(7,11), (8,11) -> Square2(8,11), (9,11) -> Square2(9,11), (10,11) -> Square2(10,11), (11,11) -> Square2(11,11), (12,11) -> Square2(12,11), (13,11) -> Square2(13,11), (14,11) -> Square2(14,11), (15,11) -> Square2(15,11), (16,11) -> Square2(16,11), (17,11) -> Square2(17,11), (18,11) -> Square2(18,11), (19,11) -> Square2(19,11), (20,11) -> Square2(20,11), (21,11) -> Square2(21,11), (22,11) -> Square2(22,11), (23,11) -> Square2(23,11), (24,11) -> Square2(24,11), (25,11) -> Square2(25,11), (26,11) -> Square2(26,11), (27,11) -> Square2(27,11), (28,11) -> Square2(28,11), (29,11) -> Square2(29,11),
    (0,12) -> Square2(0,12), (1,12) -> Square2(1,12), (2,12) -> Square2(2,12), (3,12) -> Square2(3,12), (4,12) -> Square2(4,12), (5,12) -> Square2(5,12), (6,12) -> Square2(6,12), (7,12) -> Square2(7,12), (8,12) -> Square2(8,12), (9,12) -> Square2(9,12), (10,12) -> Square2(10,12), (11,12) -> Square2(11,12), (12,12) -> Square2(12,12), (13,12) -> Square2(13,12), (14,12) -> Square2(14,12), (15,12) -> Square2(15,12), (16,12) -> Square2(16,12), (17,12) -> Square2(17,12), (18,12) -> Square2(18,12), (19,12) -> Square2(19,12), (20,12) -> Square2(20,12), (21,12) -> Square2(21,12), (22,12) -> Square2(22,12), (23,12) -> Square2(23,12), (24,12) -> Square2(24,12), (25,12) -> Square2(25,12), (26,12) -> Square2(26,12), (27,12) -> Square2(27,12), (28,12) -> Square2(28,12), (29,12) -> Square2(29,12),
    (0,13) -> Square2(0,13), (1,13) -> Square2(1,13), (2,13) -> Square2(2,13), (3,13) -> Square2(3,13), (4,13) -> Square2(4,13), (5,13) -> Square2(5,13), (6,13) -> Square2(6,13), (7,13) -> Square2(7,13), (8,13) -> Square2(8,13), (9,13) -> Square2(9,13), (10,13) -> Square2(10,13), (11,13) -> Square2(11,13), (12,13) -> Square2(12,13), (13,13) -> Square2(13,13), (14,13) -> Square2(14,13), (15,13) -> Square2(15,13), (16,13) -> Square2(16,13), (17,13) -> Square2(17,13), (18,13) -> Square2(18,13), (19,13) -> Square2(19,13), (20,13) -> Square2(20,13), (21,13) -> Square2(21,13), (22,13) -> Square2(22,13), (23,13) -> Square2(23,13), (24,13) -> Square2(24,13), (25,13) -> Square2(25,13), (26,13) -> Square2(26,13), (27,13) -> Square2(27,13), (28,13) -> Square2(28,13), (29,13) -> Square2(29,13),
    (0,14) -> Square2(0,14), (1,14) -> Square2(1,14), (2,14) -> Square2(2,14), (3,14) -> Square2(3,14), (4,14) -> Square2(4,14), (5,14) -> Square2(5,14), (6,14) -> Square2(6,14), (7,14) -> Square2(7,14), (8,14) -> Square2(8,14), (9,14) -> Square2(9,14), (10,14) -> Square2(10,14), (11,14) -> Square2(11,14), (12,14) -> Square2(12,14), (13,14) -> Square2(13,14), (14,14) -> Square2(14,14), (15,14) -> Square2(15,14), (16,14) -> Square2(16,14), (17,14) -> Square2(17,14), (18,14) -> Square2(18,14), (19,14) -> Square2(19,14), (20,14) -> Square2(20,14), (21,14) -> Square2(21,14), (22,14) -> Square2(22,14), (23,14) -> Square2(23,14), (24,14) -> Square2(24,14), (25,14) -> Square2(25,14), (26,14) -> Square2(26,14), (27,14) -> Square2(27,14), (28,14) -> Square2(28,14), (29,14) -> Square2(29,14)
  )

  def adjacent(square: (Int, Int)): Set[(Int, Int)] =
    if (square._1 < 0 || square._2 < 0) Set.empty else Set((square._1, square._2),
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

  def calcVisibleSquares(boardData2: Map[(Int, Int), Square2]) {
    boardData2.foreach(pair => {
      pair._2.radarAffected = calcSquare(pair._1)
    })
  }

  def enrichBoardData(mapData: List[Array[(String, Int)]], boardData: Map[(Int, Int), Square2]) {
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

  def kamikaze(robot: Robot, enemyRobotsData: Map[(Int, Int), List[Array[Int]]]) {
//    Console.err.println(s"${robot.i} > (${(robot.x,robot.y)})")
    val adj: List[(Int, Int)] = adjacent((robot.x, robot.y)).toList

    adj.foreach(coord => {
      val square = boardData2(coord)
      if ((square.suspect && square.ore > 0) || square.trap == 1) {
        val coordAdj = adjacent(coord)
//        Console.err.println(s"\t!suspect:$coord")
        val myRobotsCoord = myRobotList.groupBy(r => (r.x, r.y))

        val enemyRobotsNear = coordAdj.intersect(enemyRobotsData.keySet)
//        Console.err.println(s"enemy robots near")
//        enemyRobotsNear.foreach(r => Console.err.print(s"$r "))
//        Console.err.println

        val myRobotsNear = coordAdj.intersect(myRobotsCoord.keySet)
//        Console.err.println(s"my robots near")
//        myRobotsNear.foreach(r => Console.err.print(s"$r "))
//        Console.err.println


        val mySize = myRobotsNear.map(myRobotsCoord(_).size).sum
        val enemySize = enemyRobotsNear.map(enemyRobotsData(_).size).sum

//        Console.err.println(s"my size: ${mySize} enemy size: ${enemySize} ")

        if (mySize < enemySize) {
//          Console.err.println(s"kamikaze! $coord")
          robot.command = "DIG"
          robot.targetX = coord._1
          robot.targetY = coord._2
        }
      }
    })
  }

  // height: size of the map
  val Array(width, height) = for (i <- readLine split " ") yield i.toInt
//  Console.err.println(s"width=$width\theight=$height")

  var myRobotList: List[Robot] = List.empty
  var myRobotsMap: Map[Int, Robot] = Map.empty
//  var boardData = Map.empty[(Int, Int), Square]

  var moveCount = 0
  // game loop
  while (true) {
    moveCount += 1
    // myscore: Amount of ore delivered
    val Array(myscore, opponentscore) = for (i <- readLine split " ") yield i.toInt
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
    val myRobotsDataMap = entityData.withFilter(_(1) == 0).map(data => data(0) -> data).toMap
//    val enemyRobotsData = dataMap(1).map(data => (data(2), data(3)))
    val enemyRobotsData = dataMap(1).groupBy(data => (data(2), data(3)))

    if (moveCount == 1) {
      myRobotsMap = myRobotsDataMap.keys.map(key => {
        val dataset = myRobotsDataMap(key)
        key -> new Robot(dataset(0), dataset(2), dataset(3), dataset(4))
      }).toMap
//      myRobotsRaw = myRobotsData.map(dataset => new Robot(dataset(0), dataset(2), dataset(3), dataset(4)))
      myRobotList = myRobotsMap.values.toList.sortBy(_.i)
//      myRobotsMap = myRobotsRaw.map(robot => robot.i -> robot).toMap
//      boardData = createBoardData(width, height)
      calcVisibleSquares(boardData2)
    }

    enrichBoardData(mapData, boardData2)
    entityData.foreach(dataset => {
      if (dataset(2) > -1 && dataset(3) > -1) {
        val square = boardData2((dataset(2), dataset(3)))
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

//    Console.err.println(s"myscore=$myscore oppscore=$opponentscore moveCount=$moveCount")
    RobotManager.command(myRobotList, boardData2, radarcooldown, trapcooldown, (moveCount > 180 && myscore < opponentscore))

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

        kamikaze(robot, enemyRobotsData)
        println(robot.getCommand) // WAIT|MOVE x y|DIG x y|REQUEST item

       if (robot.command == "DIG" && robot.near) {
//           Console.err.println(s"!!!SET MY HOLE ${robot.i} > ${(robot.targetY, robot.targetY)}")
         val square = boardData2((robot.targetX, robot.targetY))
         square.myHole = true
         square.ore = if (square.ore > 0) square.ore - 1 else square.ore
         robot.isFlying = false
       }
      })
//    }

  }
}