//package codingames.challenge.fish

import math._
import scala.io.Source
import scala.util._
import scala.io.StdIn._

object Player extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
// val filename = "resources/fish/1.txt"
// val bufferedSource = Source.fromFile(filename)
// val data = bufferedSource.getLines
// def readInt = if (data.hasNext) data.next.toInt else { System.exit(0); -1 }
// def readLine = if (data.hasNext) data.next else { System.exit(0); "" }
//----------------------------------------------------------------------------------------------------------------------
  object Quarter extends Enumeration {
    type Quarter = Value
    val TL, TR, BL, BR = Value
  }
  import Quarter._

  trait Movable {
    def lightOn(drone: Drone): Boolean = drone.battery > 5
    def toPoint = Point(x, y)
    def x: Int
    def y: Int
  }
  case class Point(x: Int, y: Int) extends Movable
  class Creature(val id: Int, val color: Int, val ctype: Int) extends Movable {
    var x: Int = -1
    var y: Int = -1
    var creatureVx: Int = -1
    var creatureVy: Int = -1
    var visible = false
    var scannedByMe = false
    var scannedByFoe = false
  }
  case class Drone(id: Int, x: Int, y: Int, emergency: Int, battery: Int) {
    var lightOn = false
    var creatures = Set.empty[(Int, Quarter)]
    def toPoint = Point(x, y)
  }

  class Move(var isMove: Boolean, var xpos: Int, var ypos: Int, var isLightOn: Boolean) {
    def print = {
      val move = s"${if (isMove) "MOVE" else "WAIT"}"
      val xy = s" ${xpos} ${ypos}"
      val coord = s"${if (isMove) xy else ""}"
      val light = s"${if (isLightOn) " 1" else " 0"}"
      s"$move$coord$light"
    }
  }

  val radarMap = Map("TL" -> Quarter.TL, "TR" -> Quarter.TR, "BR" -> Quarter.BR, "BL" -> Quarter.BL)
  val ALLOWED_DIST = 500d
  val ENGINE_MOVE = 600d
  var prevScannedByMe = 0
  var prevScannedByFoe = 0

  var points = Set(Point(2000, 3750), Point(4000, 3750), Point(6000, 3750), Point(8000, 3750),
                   Point(2000, 6250), Point(4000, 6250), Point(6000, 6250), Point(8000, 6250),
                   Point(2000, 8750), Point(4000, 8750), Point(6000, 8750), Point(8000, 8750))

  var visitedPoints = Set.empty[Point]
  var targetPoints = Set.empty[Point]

  val rand = new Random(System.currentTimeMillis)
  val creatureCount = readLine.toInt
  // Console.err.println(s"$creatureCount")

  val creatures = for (_ <- 0 until creatureCount) yield {
    val Array(creatureId, color, _type) = (readLine split " ").filter(_ != "").map(_.toInt)
    // Console.err.println(s"$creatureId $color ${_type}")
    new Creature(creatureId, color, _type)
  }
  val creaturesMap = creatures.map(creature => (creature.id, creature)).toMap
  val monsters = creatures.filter(_.ctype == -1)

// +++++ FUNCTIONS +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  def euclidean(a: Point, b: Point): Double = sqrt(pow(b.x - a.x, 2) + pow(b.y - a.y, 2))
  def euclidean(a: (Int, Int), b: (Int, Int)): Double = sqrt(pow(b._1 - a._1, 2) + pow(b._2 - a._2, 2))
  def reachable(distance: Double) = distance > 800 && distance < 2000
  def newPoint(p1: (Int, Int), p2: (Int, Int)) = Point(p2._1 + (p2._1 - p1._1), p2._2 + (p2._2 - p1._2))
  def visitPoint(drone: Drone, p: Point) =
    if (euclidean((drone.x, drone.y), (p.x, p.y)) < 600) {
      targetPoints -= p
      (points - p, visitedPoints + p)
    }
    else (points, visitedPoints)

  def nextClosestPoint(drone: Drone, distanceMap: Map[(Drone, Point), Double]) = points.diff(targetPoints).minByOption(
    p => distanceMap((drone, p))).map(p => {
      targetPoints += p
      if (euclidean((drone.x, drone.y), (p.x, p.y)) < 600) newPoint((drone.x, drone.y), (p.x, p.y))
      else p
  })

  def avoidMonsters(drone: Drone, monsters: IndexedSeq[Creature] = monsters) = ???
  def toMove(target: Movable, drone: Drone, distanceMap: Map[(Int, Int), Double]/*, avoid: Drone => Move*/) = {
//    val dangerMonsters = monsters.map(monster => distanceMap((drone, monster)))
//      .filter(distance => distance < )
    val (dp, tp) = (drone.toPoint, target.toPoint)
    val droneTargetDist = euclidean(dp, tp)
    val nextPoint = if (droneTargetDist > ENGINE_MOVE) calculateEndPoint(dp, tp, ENGINE_MOVE) else tp
    val closestMonster = monsters.filter(_.visible).minByOption(monster => distanceMap((drone.id, monster.id)))
    closestMonster.foreach(monster => Console.err.println(s"!!!<${drone.id}> Monster=${monster.id} Dist=${distanceMap((drone.id, monster.id))}"))
//    Console.err.println(s"drone:<${drone.x},${drone.y}> " +
//                        s"target:<${target.x},${target.y}> Real:<${nextPoint.x},${nextPoint.y}>" +
//                        s" Dist:${euclidean((drone.x,drone.y),(target.x,target.y))} Dist2:${euclidean((drone.x,drone.y),(nextPoint.x,nextPoint.y))}")
    new Move(true, nextPoint.x, nextPoint.y, target.lightOn(drone))
  }
//  def toMove(target: Creature, drone: Drone) = new Move(true, target.x, target.y, target.lightOn(drone))
//  new Move(true, creature.x, creature.y, drone.battery > 5 && drone.lightOn)
  def addDroneCreature(drone: Drone, creatureId: Int, radar: String): Unit = {
    val pair = (creatureId, radarMap(radar))
    drone.creatures = drone.creatures + pair
  }
  def resurfacePointOpt(drone: Drone) = {
    val myScanned = creatures.count(_.scannedByMe)
    if (myScanned == creatureCount/* || prevScannedByMe < myScanned*/) {
//      Console.err.println(s"Need resurface! prevScannedByMe=$prevScannedByMe scannedCreatures=${creatures.count(_.scannedByMe)}")
      Some(Point(drone.x, 499))
    } else {
//      Console.err.println(s"No need resurface. prevScannedByMe=$prevScannedByMe scannedCreatures=${creatures.count(_.scannedByMe)}")
      None
    }
  }
  def calculateEndPoint(startPoint: Point, targetPoint: Point, distance: Double) = {
    val distX = targetPoint.x - startPoint.x
    val distY = targetPoint.y - startPoint.y
    val distXY = euclidean(startPoint, targetPoint)
    val angleX = distX / distXY
    val angleY = distY / distXY
    val newX = distance * angleX
    val newY = distance * angleY
    Point(startPoint.x + Math.round(newX).toInt, startPoint.y + Math.round(newY).toInt)
  }
  def shortestDistance(point: Creature, drone: Drone, target: Point): Double = {
    val numerator = abs((target.y - drone.y) * point.x - (target.x - drone.x) * point.y + target.x * drone.y - target.y * drone.x)
    val denominator = sqrt((target.y - drone.y) * (target.y - drone.y) + (target.x - drone.x) * (target.x - drone.x))
    numerator / denominator
  }
  def recalculatePoint(drone: Drone, target: Point) = {
    val me = (3,2)
    val monster = (6,5)
    val meMonsterBasis = (monster._1 - drone.x, monster._2 - drone.y)
    val meMonsterLength = euclidean(me, monster)
    val (x,y) = meMonsterBasis

    val sinA = ALLOWED_DIST / meMonsterLength
    val angle = asin(sinA)
    val rotatedX = Math.round(me._1 + x * cos(angle) - y * sin(angle))
    val rotatedY = Math.round(me._2 + x * sin(angle) + y * cos(angle))
    (rotatedX, rotatedY)
  }
  def resurface(drone: Drone, score: Int, forMe: Boolean) {
    if (drone.y < 500) {
      if (forMe) {
        val byMeNum = creatures.count(_.scannedByMe)
        // Console.err.println(s"RESURFACED! prevScannedByMe=$prevScannedByMe scannedCreatures=$byMeNum")
        prevScannedByMe = byMeNum
      } else {
        val byFoeNum = creatures.count(_.scannedByFoe)
        if (byFoeNum > prevScannedByFoe) prevScannedByFoe = byFoeNum
      }
    }
  }
// ----- FUNCTIONS -----------------------------------------------------------------------------------------------------


  while (true) {
    val myScore = readLine.toInt
    // Console.err.println(s"$myScore")
    val foeScore = readLine.toInt
    // Console.err.println(s"$foeScore")
    val myScanCount = readLine.toInt
    // Console.err.println(s"$myScanCount")
    val myCreatures = for (i <- 0 until myScanCount) yield {
      val creatureId = readLine.toInt
      // Console.err.println(s"$creatureId")
      i
    }
    val foeScanCount = readLine.toInt
    // Console.err.println(s"$foeScanCount")
    val foeCreatures = for (i <- 0 until foeScanCount) yield {
      val creatureId = readLine.toInt
      // Console.err.println(s"$creatureId")
      i
    }
    val myDroneCount = readLine.toInt
    // Console.err.println(s"$myDroneCount")

    val myDronesMap = (for (i <- 0 until myDroneCount) yield {
      val Array(droneId, droneX, droneY, emergency, battery) = (readLine split " ").filter(_ != "").map(_.toInt)
      // Console.err.println(s"$droneId $droneX $droneY $emergency $battery")
      (droneId, Drone(droneId, droneX, droneY, emergency, battery))
    }).toMap
    val myDrones = myDronesMap.values.toList

    val foeDroneCount = readLine.toInt
    // Console.err.println(s"$foeDroneCount")
    val foeDronesMap = (for (i <- 0 until foeDroneCount) yield {
      val Array(droneId, droneX, droneY, emergency, battery) = (readLine split " ").filter(_ != "").map(_.toInt)
      // Console.err.println(s"$droneId $droneX $droneY $emergency $battery")
      (droneId, Drone(droneId, droneX, droneY, emergency, battery))
    }).toMap
    val foeDrones = foeDronesMap.values.toList

//------- ENRICH -------------------------------------------------------------------------------------------------------

    val droneScanCount = readLine.toInt
    // Console.err.println(s"$droneScanCount")
    for (_ <- 0 until droneScanCount) {
      val Array(droneId, creatureId) = (readLine split " ").filter(_ != "").map(_.toInt)
      myDronesMap.get(droneId).foreach(_ => creaturesMap(creatureId).scannedByMe = true)
      foeDronesMap.get(droneId).foreach(_ => creaturesMap(creatureId).scannedByFoe = true)
      // Console.err.println(s"$droneId $creatureId")
    }
    val visibleCreatureCount = readLine.toInt
    // Console.err.println(s"$visibleCreatureCount")

    creatures.foreach(creature => creature.visible = false)
    for (i <- 0 until visibleCreatureCount) {
      val Array(creatureId, creatureX, creatureY, creatureVx, creatureVy) = (readLine split " ").filter(_ != "").map(_.toInt)
      // Console.err.println(s"$creatureId $creatureX $creatureY $creatureVx $creatureVy")
      val creature = creaturesMap(creatureId)
      creature.x = creatureX
      creature.y = creatureY
      creature.creatureVx = creatureVx
      creature.creatureVy = creatureVy
      creature.visible = true
    }
    val radarBlipCount = readLine.toInt
    // Console.err.println(s"$radarBlipCount")
    for (i <- 0 until radarBlipCount) {
      val Array(_droneId, _creatureId, radar) = readLine split " "
      val did = _droneId.toInt
      val cid = _creatureId.toInt


      myDronesMap.get(did).foreach(drone => addDroneCreature(drone, cid, radar))
      foeDronesMap.get(cid).foreach(drone => addDroneCreature(drone, cid, radar))

      // Console.err.println(s"${_droneId} ${_creatureId} $radar")
      val droneId = _droneId.toInt
      val creatureId = _creatureId.toInt
    }

//------- END OF ENTRY -------------------------------------------------------------------------------------------------

    if (points.isEmpty) points = visitedPoints

    targetPoints = Set.empty

    val creatureDroneDistanceMap = (for (drone <- myDrones; creature <- creatures; if creature.visible) yield {
      val distance = euclidean((drone.x, drone.y), (creature.x, creature.y))
      ((drone.id, creature.id), distance)
    }).toMap

    val dronePointsDistanceMap = (for (point <- points; drone <- myDrones) yield {
      val distance = euclidean((drone.x, drone.y), (point.x, point.y))
      ((drone, point), distance)
    }).toMap

    val interestingCreatures = creatures.filter(creature => creature.visible && !creature.scannedByMe && creature.ctype > -1)
//    // Console.err.println(s"Interesting creatures size=${interestingCreatures.length}")
//    // Console.err.println(s"Visited points=${visitedPoints}")







    myDrones.foreach(drone => {
      points.foreach(point => {
        val (newPoints, newVisitedPoints) = visitPoint(drone, point)
        points = newPoints
        visitedPoints = newVisitedPoints
      })

      val closestCreature = interestingCreatures.minByOption(creature => creatureDroneDistanceMap.get((drone.id, creature.id)))
//      drone.lightOn = interestingCreatures.exists(creature => reachable(creatureDroneDistanceMap.get((drone.id, creature.id))))
//      if (drone.lightOn) {
         // Console.err.println(s"Dron-${drone.id} sees interesting creature! Drone battery=${drone.battery}")
//      }
      val move =
        closestCreature.map(creature => toMove(creature, drone, creatureDroneDistanceMap))
        .orElse(resurfacePointOpt(drone).map(point => toMove(point, drone, creatureDroneDistanceMap)))
        .orElse(nextClosestPoint(drone, dronePointsDistanceMap).map(point => toMove(point, drone, creatureDroneDistanceMap)))
        .orElse(Some(new Move(false, -1, -1, drone.battery > 5)))
//        .map()
        .get

      println(s"${move.print} ::drone ${drone.id} <${drone.x},${drone.y}> moves. Battery=${drone.battery}")

//      resurface(drone, myScore, true)
    })
  }
}