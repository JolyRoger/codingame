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

  case class Point(x: Int, y: Int)
  class Creature(val id: Int, val color: Int, val ctype: Int) {
    var x: Int = -1
    var y: Int = -1
    var creatureVx: Int = -1
    var creatureVy: Int = -1
    var visible = false
    var scannedByMe = false
    var scannedByFoe = false
    def toPoint = Point(x, y)
    def nextPoint = Point(x + creatureVx, y + creatureVy)
  }
  case class Drone(id: Int, x: Int, y: Int, emergency: Int, battery: Int) {
    var scanned = Set.empty[Int]
    var saved = Set.empty[Int]
    var creatures = Map.empty[Int, Quarter]
    var message: String = _
    def toPoint = Point(x, y)
  }

  class Move(var isMove: Boolean, var x: Int, var y: Int, var isLightOn: Boolean) {
    var danger = false
    def toPoint = Point(x ,y)
    def print = {
      val move = s"${if (isMove) "MOVE" else "WAIT"}"
      val xy = s" $x $y"
      val coord = s"${if (isMove) xy else ""}"
      val light = s"${if (isLightOn) " 1" else " 0"}"
      s"$move$coord$light"
    }
  }

  var dronePrevLight = Map.empty[Int, Point]
  var prevDrones = Map.empty[Int, Drone]
  var stageFirst = Map.empty[Int, Boolean]
  var leftRightDrone = Map.empty[Int, Boolean]
  val radarMap = Map("TL" -> Quarter.TL, "TR" -> Quarter.TR, "BR" -> Quarter.BR, "BL" -> Quarter.BL)
  val ALLOWED_DIST = 520d
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

  def searchStandardRectangle(drone: Drone, friend: Drone, creature: Creature) = {
    var leftBorder = 0
    var rightBorder = 10000
    var minDeep = 0
    var maxDeep = 10000

    val myDirection = drone.creatures(creature.id)
    val friendDirection = friend.creatures(creature.id)
    val fishType = creature.ctype

    myDirection match {
      case TR => leftBorder = drone.x
        maxDeep = drone.y
      case BR => leftBorder = drone.x
        minDeep = drone.y
      case TL => rightBorder = drone.x
        maxDeep = drone.y
      case BL => rightBorder = drone.x
        minDeep = drone.y
  }
  friendDirection match {
    case TR => leftBorder = max(leftBorder, friend.x)
      maxDeep = min(maxDeep, friend.y)
    case BR => leftBorder = max(leftBorder, friend.x)
      minDeep = max(minDeep, friend.y)
    case TL => rightBorder = min(rightBorder, friend.x)
      maxDeep = min(maxDeep, friend.y)
    case BL => rightBorder = min(rightBorder, friend.x)
      minDeep = max(minDeep, friend.y)
    }
    if (fishType == 0) {
      minDeep = max(minDeep, 2500)
      maxDeep = min(maxDeep, 5000)
    } else if (fishType == 1) {
      minDeep = max(minDeep, 5000)
      maxDeep = min(maxDeep, 7500)
    } else if (fishType == 2) {
      minDeep = max(minDeep, 7500)
      maxDeep = min(maxDeep, 10000)
    }
    (leftBorder, minDeep, rightBorder, maxDeep)
  }
  def searchRectangle(drone: Drone, friend: Drone, creature: Creature) = {
    val (left, minHeight, right, maxHeight) = searchStandardRectangle(drone, friend, creature)
    var leftBorder = left
    var rightBorder = right
    var minDeep = minHeight
    var maxDeep = maxHeight
    val myDirection = drone.creatures(creature.id)

    val prevDroneOpt = prevDrones.get(drone.id)
    prevDroneOpt match {
      case Some(prevDrone) =>
        val prevDirection = prevDrone.creatures(creature.id)
        if ((prevDirection == BL && myDirection == BR) || (prevDirection == TL && myDirection == TR) ||
            (prevDirection == BR && myDirection == BL) && (prevDirection == TR && myDirection == TL)) {
          leftBorder = min(drone.x, prevDrone.x)
          rightBorder = max(drone.x, prevDrone.x)
        } else if ((prevDirection == BR && myDirection == TR) || (prevDirection == BL && myDirection == TL) ||
                   (prevDirection == TR && myDirection == BR) && (prevDirection == TL && myDirection == BL)) {
          minDeep = min(prevDrone.y, drone.y)
          maxDeep = max(prevDrone.y, drone.y)
        }
      case None =>
    }
    (Point(leftBorder, minDeep), Point(rightBorder, maxDeep))
  }

  def nextClosestPoint(myFriend: Drone)(drone: Drone) = {
    val (myPriority, friendPriority) = if (leftRightDrone(drone.id)) (Set(BL, TL), Set(BR, TR)) else (Set(BR, TR), Set(BL, TL))
    def friendResponsibility(fish: Creature) = friendPriority.contains(myFriend.creatures(fish.id)) && myFriend.emergency == 0

    creatures.filterNot(creature => creature.ctype == -1 || creature.scannedByMe || targetCreature.contains(creature.id) || !drone.creatures.contains(creature.id) || friendResponsibility(creature))
      .minByOption(creature => {
        var index = 0
        val zone = drone.creatures(creature.id)
        val friendZone = myFriend.creatures(creature.id)
        index = if (myPriority.contains(zone)) 0 else 2
        index = if (friendPriority.contains(friendZone)) index + 10 else index
        index + 2 - creature.ctype
      })
      .map(creature => {
        targetCreature += creature.id
        val (tlPoint, brPoint) = searchRectangle(drone, myFriend, creature)
        drone.message = s"Drone-${drone.id} hunts creature <${creature.id}> in area <$tlPoint $brPoint>"
        val middleX = (brPoint.x - tlPoint.x) / 2 + tlPoint.x
        val middleY = (brPoint.y - tlPoint.y) / 2 + tlPoint.y
        Point(middleX, middleY)
    })
  }
  def toMove(target: Point, drone: Drone, monsters: IndexedSeq[Creature], step: Int) = {
    val (dp, tp) = (drone.toPoint, target)
    val droneTargetDist = euclidean(dp, tp)
    val nextPoint = if (droneTargetDist > ENGINE_MOVE) calculateEndPoint(dp, tp, ENGINE_MOVE) else tp
    val danger = monsters.exists(m => checkForDanger(m, drone, nextPoint))

    val newMove = if (danger) {
      val move = processDangers(drone, monsters, nextPoint)
      move.danger = true
      drone.message = s"Drone-${drone.id} avoids monster(s) <${monsters.map(_.id).mkString(",")}>"
      move
    } else {
      new Move(true, nextPoint.x, nextPoint.y, false)
    }
    newMove.isLightOn = calculateLight(drone, step, newMove, target)
    newMove
  }

  def addDroneCreature(drone: Drone, creatureId: Int, radar: String) {
    val pair = (creatureId, radarMap(radar))
    drone.creatures = drone.creatures + pair
  }
  def calculateLight(drone: Drone, step: Int, move: Move, realTarget: Point) = {
    val closeToTarget = euclidean(drone.toPoint, realTarget) < 2000d
    val inTheMiddleOfLevel = (drone.y > 3550 - 300 && drone.y < 3550 + 300) ||
                             (drone.y > 6050 - 300 && drone.y < 6050 + 300)
    drone.battery > 4 &&
    step > 2 && ((stageFirst(drone.id) && inTheMiddleOfLevel) ||
                (!stageFirst(drone.id) && closeToTarget))
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
    val monsterPath = splitBy(monster.toPoint, monster.nextPoint)
    val myPath = splitBy(drone.toPoint, droneNextPoint)
    myPath.zip(monsterPath).exists(ab => euclidean(ab._1, ab._2) < ALLOWED_DIST)
  }
  def getRotatedPoint(startPoint: Point, targetPoint: Point, angle: Double) = {
    val (x, y) = (targetPoint.x - startPoint.x, targetPoint.y - startPoint.y)
    val cosAngle = cos(angle)
    val sinAngle = sin(angle)
    val rotatedX = Math.round(startPoint.x + x * cosAngle - y * sinAngle).toInt
    val rotatedY = Math.round(startPoint.y + x * sinAngle + y * cosAngle).toInt
    Point(rotatedX, rotatedY)
  }
  def recalculateMoving(drone: Drone, myTarget: Point) = {
    val points = for (i <- 0 until 8) yield getRotatedPoint(drone.toPoint, myTarget, i * Pi / 4)
    points.filter(valid)
  }
  def processDangers(drone: Drone, dangerMonsters: IndexedSeq[Creature], nextPoint: Point) = {
    Console.err.println(s"Danger monsters found!")
    def filterPoints(points: IndexedSeq[Point]) = points.filterNot(nextPointCandidate => dangerMonsters.exists(monster => checkForDanger(monster, drone, nextPointCandidate)))

    var safePoints = filterPoints(recalculateMoving(drone, Point(drone.x, drone.y - 600)))
    if (safePoints.isEmpty) safePoints = filterPoints(recalculateMoving(drone, nextPoint))

    safePoints.minByOption(pointCandidate => euclidean(pointCandidate, nextPoint))
      .map(p => new Move(true, p.x, p.y, false))
      .getOrElse(new Move(true, drone.x, 499, false))
  }
  def resurfacePointOpt(drone: Drone) = {
//    val pOpt = if (creatures.count(_.scannedByMe) >= drone.creatures.keys.size - 2
    val pOpt = if (creatures.count(_.scannedByMe) == creatureCount
//      || drone.saved.size + 2 < drone.scanned.size && drone.y < 5000
      || drone.saved.size + 4 < drone.scanned.size/* && drone.y < 7500*/
    ) Some(Point(drone.x, 499))
    else None
    pOpt.foreach(p => drone.message = s"Drone-${drone.id} resurfaces to <$p>")
    pOpt
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
        leftRightDrone += (droneId -> (droneX < 5000))
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
    for (_ <- 0 until visibleCreatureCount) {
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
    for (_ <- 0 until radarBlipCount) {
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
    targetCreature = Set.empty
    val monsters = creatures.filter(monster => monster.ctype == -1 && monster.x > -1)

    myDrones.foreach(drone => {
      if (drone.emergency == 1) drone.scanned = drone.saved
      val myFriend = (myDronesMap - drone.id).head._2

      stageFirst += stageFirst.get(drone.id).map(stage1 => {
        if (stage1 && drone.y > 7500) drone.id -> false
        else drone.id -> stage1
      }).getOrElse(drone.id -> true)

      points.foreach(point => {
        val (newPoints, newVisitedPoints) = visitPoint(drone.toPoint, point)
        points = newPoints
        visitedPoints = newVisitedPoints
      })

//      val nextTargetPoint: Drone => Option[Point] = if (stageFirst(drone.id)) deepestPoint else nextClosestPoint2(myFriend)

      val move = resurfacePointOpt(drone).map(point => toMove(point, drone, monsters, step))
//        .orElse(nextTargetPoint(drone).map(point => toMove(point, drone, monsters)))
        .orElse(nextClosestPoint(myFriend)(drone).map(point => toMove(point, drone, monsters, step)))
        .orElse(Some(Point(drone.x, 499)).map(point => toMove(point, drone, monsters, step)))
        .orElse(Some(new Move(false, -1, -1, false)))
        .get

      if (move.isLightOn) dronePrevLight += drone.id -> move.toPoint
      resurface(drone)

      println(s"${move.print} :: ${drone.message}")
    })
    prevDrones = myDronesMap
  }
}