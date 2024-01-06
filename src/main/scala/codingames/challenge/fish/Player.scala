package codingames.challenge.fish

import math._
import scala.io.Source
import scala.util._
import scala.io.StdIn._

object Player extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
  val filename = "resources/fish/1.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines
  def readInt = if (data.hasNext) data.next.toInt else { System.exit(0); -1 }
  def readLine = if (data.hasNext) data.next else { System.exit(0); "" }
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
    def nextPoint = Point(x + creatureVx, y + creatureVy)
    def moving = creatureVx > 0 || creatureVy > 0
  }
  case class Drone(id: Int, x: Int, y: Int, emergency: Int, battery: Int) {
    var lightOn = false
    var scanned = Set.empty[Int]
    var saved = Set.empty[Int]
    var creatures = Set.empty[(Int, Quarter)]
    var targetPoints = Set.empty[Point]
    var visitedPoints = Set.empty[Point]
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

  var leftRightDrone = Map.empty[Int, Point => Boolean]
  val radarMap = Map("TL" -> Quarter.TL, "TR" -> Quarter.TR, "BR" -> Quarter.BR, "BL" -> Quarter.BL)
  val ALLOWED_DIST = 520d
  val AGGRESSIVE_SPEED = 540d
  val PEACEFUL_SPEED = 270d
  val ENGINE_MOVE = 600d

  var points = Set(Point(2000, 3750), Point(4000, 3750), Point(6000, 3750), Point(8000, 3750),
                   Point(2000, 6250), Point(4000, 6250), Point(6000, 6250), Point(8000, 6250),
                   Point(2000, 8750), Point(4000, 8750), Point(6000, 8750), Point(8000, 8750))

  var visitedPoints = Set.empty[Point]
  var targetPoints = Set.empty[Point]
  var targetCreature = Set.empty[Int]

  val creatureCount = readLine.toInt
  Console.err.println(s"$creatureCount")

  val creatures = for (_ <- 0 until creatureCount) yield {
    val Array(creatureId, color, _type) = (readLine split " ").filter(_ != "").map(_.toInt)
    Console.err.println(s"$creatureId $color ${_type}")
    new Creature(creatureId, color, _type)
  }
  val creaturesMap = creatures.map(creature => (creature.id, creature)).toMap


// +++++ FUNCTIONS +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  def euclidean(a: Point, b: Point): Double = hypot(b.x - a.x, b.y - a.y)
  def valid(p: Point) = p.x >= 0 && p.x < 10000 && p.y >= 0 && p.y < 10000
  def reachable(distance: Double) = distance > 800 && distance < 2000
  def newPoint(p1: (Int, Int), p2: (Int, Int)) = Point(p2._1 + (p2._1 - p1._1), p2._2 + (p2._2 - p1._2))
  def visitPoint(drone: Point, p: Point) =
    if (euclidean(drone, p) < 600) {
      targetPoints -= p
      (points - p, visitedPoints + p)
    }
    else (points, visitedPoints)

  def nextClosestPoint2(drone: Drone) = {
    val targetDirection = drone.creatures
      .filterNot(creatureIdRadar => creaturesMap(creatureIdRadar._1).scannedByMe)
      .groupBy(_._2)
      .maxBy(_._2.size)._1
    Some {
      targetDirection match {
        case TL => Point(drone.x / 2, drone.y / 2)
        case TR => Point((10000 - drone.x) / 2 + drone.x, drone.y / 2)
        case BL => Point(drone.x, (10000 - drone.y) / 2 + drone.y)
        case BR => Point((10000 - drone.x) / 2 + drone.x, (10000 - drone.y) / 2 + drone.y)
      }
    }
  }
  def leftPoint(p: Point): Boolean = p.x < 5000
  def rightPoint(p: Point): Boolean = p.x >= 5000
  def nextClosestPoint(drone: Drone, distanceMap: Map[(Drone, Point), Double]) = {
    def process(p: Point) = {
        targetPoints += p
        if (euclidean(drone.toPoint, p) < 600) newPoint((drone.x, drone.y), (p.x, p.y))
        else p
    }
    val unvisited = points.diff(targetPoints)
    val (my, other) = unvisited.partition(p => leftRightDrone(drone.id)(p))
    my.maxByOption(_.y)
      .orElse(other.maxByOption(_.y))
      .orElse(unvisited.minByOption(p => distanceMap((drone, p))))
      .map(process)
  }
  def toMove(target: Movable, drone: Drone, monsters: IndexedSeq[Creature], firstStep: Boolean = false) = {
//    val dangerMonsters = monsters.map(monster => distanceMap((drone, monster)))
//      .filter(distance => distance < )
    val (dp, tp) = (drone.toPoint, target.toPoint)
    val droneTargetDist = euclidean(dp, tp)
    val nextPoint = if (droneTargetDist > ENGINE_MOVE) calculateEndPoint(dp, tp, ENGINE_MOVE) else tp
    val dangerMonsters = monsters.filter(m => checkForDanger(m, drone, nextPoint))

//    closestMonster.foreach(monster => Console.err.println(s"!!!<${drone.id}> Monster=${monster.id} Dist=${distanceMap((drone.id, monster.id))}"))
//    Console.err.println(s"drone:<${drone.x},${drone.y}> " +
//                        s"target:<${target.x},${target.y}> Real:<${nextPoint.x},${nextPoint.y}>" +
//                        s" Dist:${euclidean((drone.x,drone.y),(target.x,target.y))} Dist2:${euclidean((drone.x,drone.y),(nextPoint.x,nextPoint.y))}")
    if (dangerMonsters.isEmpty)
      new Move(true, nextPoint.x, nextPoint.y, !firstStep && target.lightOn(drone))
    else {
      processDangers(drone, dangerMonsters, nextPoint, target)
    }
  }

//  def toMove(target: Creature, drone: Drone) = new Move(true, target.x, target.y, target.lightOn(drone))
//  new Move(true, creature.x, creature.y, drone.battery > 5 && drone.lightOn)
  def addDroneCreature(drone: Drone, creatureId: Int, radar: String) {
    val pair = (creatureId, radarMap(radar))
    drone.creatures = drone.creatures + pair
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
  def splitBy(startPoint: Point, targetPoint: Point) = {
    if (startPoint == targetPoint) {
      List(startPoint, startPoint, startPoint, startPoint, startPoint)
    } else {
      val halfDistance = euclidean(startPoint, targetPoint) / 2
      val quarterDistance = halfDistance / 2
      val middle = calculateEndPoint(startPoint, targetPoint, halfDistance)
      val second = calculateEndPoint(startPoint, middle, quarterDistance)
      val fourth = calculateEndPoint(middle, targetPoint, quarterDistance)
      List(startPoint, second, middle, fourth, targetPoint)
    }
  }
  def checkForDanger(monster: Creature, drone: Drone, droneNextPoint: Point) = {
    monster.x > -1 && {
      val monsterPath = splitBy(monster.toPoint, monster.nextPoint)
      val myPath = splitBy(drone.toPoint, droneNextPoint)
      myPath.zip(monsterPath).exists(ab => euclidean(ab._1, ab._2) < ALLOWED_DIST)
    }
  }
  def getRotatedPoint(startPoint: Point, targetPoint: Point, angle: Double) = {
    val (x, y) = (targetPoint.x - startPoint.x, targetPoint.y - startPoint.y)
    val cosAngle = cos(angle)
    val sinAngle = sin(angle)
    val rotatedX = Math.round(startPoint.x + x * cosAngle - y * sinAngle).toInt
    val rotatedY = Math.round(startPoint.y + x * sinAngle + y * cosAngle).toInt
    Point(rotatedX, rotatedY)
  }
  def recalculateMoving(drone: Drone, monster: Creature, myTarget: Point) = {
    val points = for (i <- 0 until 8) yield getRotatedPoint(drone.toPoint, myTarget, i * Pi / 4)
    points.filter(valid)
  }
  def processDangers(drone: Drone, dangerMonsters: IndexedSeq[Creature], nextPoint: Point, target: Movable) = {
    Console.err.println(s"Danger monsters found!")
    val monsterPoints = dangerMonsters.flatMap(monster => {
      val np = recalculateMoving(drone, monster, nextPoint)
      Console.err.println(s"Drone=from <${drone.id}> to <${drone.toPoint}>\tMonster=from<${monster.id}> to <${monster.toPoint}>")
      np
    })
    monsterPoints.filterNot(nextPointCandidate => dangerMonsters.exists(monster => checkForDanger(monster, drone, nextPointCandidate)))
      .minByOption(pointCandidate => euclidean(pointCandidate, nextPoint))
      .map(p => new Move(true, p.x, p.y, target.lightOn(drone)))
      .getOrElse(new Move(false, -1, -1, false))
  }
  def resurfacePointOpt(drone: Drone) = {
    if (creatures.count(_.scannedByMe) == creatureCount ||
        drone.saved.size + 2 < drone.scanned.size && drone.y < 5000 ||
        drone.saved.size + 4 < drone.scanned.size && drone.y < 7500) Some(Point(drone.x, 499))
    else None
  }
  def resurface(drone: Drone) {
    if (drone.y < 500) drone.saved = drone.scanned
  }
  def calculateNextMoveSpeed(creature: Creature, distIncrement: Double) = {
    if (!creature.visible) {
      None
    } else if (creature.ctype > -1 || (creature.creatureVx == 0 && creature.creatureVy == 0)) {
      Some(creature.toPoint)
    } else {
      val basis = Point(creature.creatureVx, creature.creatureVy)
      val oldDist = euclidean(creature.toPoint, creature.nextPoint)
      val newDist = oldDist + distIncrement
      val angleX = basis.x / oldDist  // cos a
      val angleY = basis.y / oldDist  // sin a

      val newx = Math.round(creature.x + angleX * newDist).toInt
      val newy =  Math.round(creature.y + angleY * newDist).toInt
      Some(Point(newx, newy))
    }
  }
// ----- FUNCTIONS -----------------------------------------------------------------------------------------------------
  for (step <- LazyList.from(0).takeWhile(_ < 201)) {
    val myScore = readLine.toInt
    Console.err.println(s"$myScore")
    val foeScore = readLine.toInt
    Console.err.println(s"$foeScore")
    val myScanCount = readLine.toInt
    Console.err.println(s"$myScanCount")
    val myCreatures = for (i <- 0 until myScanCount) yield {
      val creatureId = readLine.toInt
      Console.err.println(s"$creatureId")
      i
    }
    val foeScanCount = readLine.toInt
    Console.err.println(s"$foeScanCount")
    val foeCreatures = for (i <- 0 until foeScanCount) yield {
      val creatureId = readLine.toInt
      Console.err.println(s"$creatureId")
      i
    }
    val myDroneCount = readLine.toInt
    Console.err.println(s"$myDroneCount")

    val myDronesMap = (for (_ <- 0 until myDroneCount) yield {
      val Array(droneId, droneX, droneY, emergency, battery) = (readLine split " ").filter(_ != "").map(_.toInt)
      Console.err.println(s"$droneId $droneX $droneY $emergency $battery")
      if (step == 0) {
        val splitf: Point => Boolean = if (droneX < 5000) leftPoint else rightPoint
        leftRightDrone += (droneId -> splitf)
      }
      (droneId, Drone(droneId, droneX, droneY, emergency, battery))
    }).toMap
    val myDrones = myDronesMap.values.toList
    val foeDroneCount = readLine.toInt
    Console.err.println(s"$foeDroneCount")
    val foeDronesMap = (for (i <- 0 until foeDroneCount) yield {
      val Array(droneId, droneX, droneY, emergency, battery) = (readLine split " ").filter(_ != "").map(_.toInt)
      Console.err.println(s"$droneId $droneX $droneY $emergency $battery")
      (droneId, Drone(droneId, droneX, droneY, emergency, battery))
    }).toMap
    val foeDrones = foeDronesMap.values.toList

//------- ENRICH -------------------------------------------------------------------------------------------------------

    val droneScanCount = readLine.toInt
    Console.err.println(s"$droneScanCount")
    for (_ <- 0 until droneScanCount) {
      val Array(droneId, creatureId) = (readLine split " ").filter(_ != "").map(_.toInt)
      myDronesMap.get(droneId).foreach(drone => {
        creaturesMap(creatureId).scannedByMe = true
        drone.scanned += creatureId
      })
      foeDronesMap.get(droneId).foreach(drone => {
        creaturesMap(creatureId).scannedByFoe = true
        drone.scanned += creatureId
      })
      Console.err.println(s"$droneId $creatureId")
    }
    val visibleCreatureCount = readLine.toInt
    Console.err.println(s"$visibleCreatureCount")

    creatures.foreach(creature => {
      val nextMovePoint = calculateNextMoveSpeed(creature, PEACEFUL_SPEED)
      if (creature.creatureVx > -1) {
        creature.x += creature.creatureVx
        creature.y += creature.creatureVy
      } else {
        creature.x = -1
        creature.y = -1
      }
      nextMovePoint match {
        case Some(p) =>
          creature.creatureVx = p.x - creature.x
          creature.creatureVy = p.y - creature.y
        case None =>
          creature.creatureVx = -1
          creature.creatureVy = -1
      }
      creature.visible = false
    })
    for (i <- 0 until visibleCreatureCount) {
      val Array(creatureId, creatureX, creatureY, creatureVx, creatureVy) = (readLine split " ").filter(_ != "").map(_.toInt)
      Console.err.println(s"$creatureId $creatureX $creatureY $creatureVx $creatureVy")
      val creature = creaturesMap(creatureId)
      creature.x = creatureX
      creature.y = creatureY
      creature.creatureVx = creatureVx
      creature.creatureVy = creatureVy
      creature.visible = true
    }
    val radarBlipCount = readLine.toInt
    Console.err.println(s"$radarBlipCount")
    for (i <- 0 until radarBlipCount) {
      val Array(_droneId, _creatureId, radar) = readLine split " "
      val did = _droneId.toInt
      val cid = _creatureId.toInt


      myDronesMap.get(did).foreach(drone => addDroneCreature(drone, cid, radar))
      foeDronesMap.get(cid).foreach(drone => addDroneCreature(drone, cid, radar))

      Console.err.println(s"${_droneId} ${_creatureId} $radar")
    }

//------- END OF ENTRY -------------------------------------------------------------------------------------------------

    if (points.isEmpty) points = visitedPoints

    targetPoints = Set.empty
    val monsters = creatures.filter(_.ctype == -1)

    val creatureDroneDistanceMap = (for (drone <- myDrones; creature <- creatures; if creature.visible) yield {
      val distance = euclidean(drone.toPoint, creature.toPoint)
      ((drone.id, creature.id), distance)
    }).toMap

    val dronePointsDistanceMap = (for (point <- points; drone <- myDrones) yield {
      val distance = euclidean(drone.toPoint, point)
      ((drone, point), distance)
    }).toMap

    val interestingCreatures = creatures.filter(creature => creature.visible && !creature.scannedByMe && creature.ctype > -1)

    myDrones.foreach(drone => {
      if (drone.emergency == 1) drone.scanned = drone.saved

      points.foreach(point => {
        val (newPoints, newVisitedPoints) = visitPoint(drone.toPoint, point)
        points = newPoints
        visitedPoints = newVisitedPoints
      })

      val closestCreature = interestingCreatures.minByOption(creature => creatureDroneDistanceMap.get((drone.id, creature.id)))

      val move = closestCreature.map(creature => toMove(creature, drone, monsters))
        .orElse(resurfacePointOpt(drone).map(point => toMove(point, drone, monsters)))
//        .orElse(nextClosestPoint2(drone).map(point => toMove(point, drone, monsters, step < 2)))
        .orElse(nextClosestPoint(drone, dronePointsDistanceMap).map(point => toMove(point, drone, monsters, step < 2)))
        .orElse(Some(Point(drone.x, 499)).map(point => toMove(point, drone, monsters)))
        .orElse(Some(new Move(false, -1, -1, false)))
        .get

      println(s"${move.print} ::drone ${drone.id} <${drone.x},${drone.y}> Step=$step Battery=${drone.battery}")

      resurface(drone)
    })
  }
}