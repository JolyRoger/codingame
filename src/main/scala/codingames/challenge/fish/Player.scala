//package codingames.challenge.fish

import math._
import scala.io.Source
import scala.util._
import scala.io.StdIn._

/**
 * Score points by scanning valuable fish faster than your opponent.
 **/
object Player extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
//  val filename = "resources/fish/1.txt"
//  val bufferedSource = Source.fromFile(filename)
//  val data = bufferedSource.getLines
//  def readInt = if (data.hasNext) data.next.toInt else { System.exit(0); -1 }
//  def readLine = if (data.hasNext) data.next else { System.exit(0); "" }
//----------------------------------------------------------------------------------------------------------------------
  object Quarter extends Enumeration {
    type Quarter = Value
    val TL, TR, BL, BR = Value
  }
  import Quarter._

  case class InterestingPoint(x: Int, y: Int)
  class Creature(val id: Int, val color: Int, val ctype: Int) {
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

  var prevScannedByMe = 0
  var prevScannedByFoe = 0

  var points = Set(InterestingPoint(2000, 3750), InterestingPoint(4000, 3750), InterestingPoint(6000, 3750), InterestingPoint(8000, 3750),
                   InterestingPoint(2000, 6250), InterestingPoint(4000, 6250), InterestingPoint(6000, 6250), InterestingPoint(8000, 6250),
                   InterestingPoint(2000, 8750), InterestingPoint(4000, 8750), InterestingPoint(6000, 8750), InterestingPoint(8000, 8750))

  var visitedPoints = Set.empty[InterestingPoint]

  val rand = new Random(System.currentTimeMillis)
  val creatureCount = readLine.toInt
  // Console.err.println(s"$creatureCount")

  val creatures = for (_ <- 0 until creatureCount) yield {
    val Array(creatureId, color, _type) = (readLine split " ").filter(_ != "").map(_.toInt)
    // Console.err.println(s"$creatureId $color ${_type}")
    new Creature(creatureId, color, _type)
  }
  val creaturesMap = creatures.map(creature => (creature.id, creature)).toMap

// +++++ FUNCTIONS +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  def euclidean(a: (Int, Int), b: (Int, Int)): Double = sqrt(pow(b._1 - a._1, 2) + pow(b._2 - a._2, 2))
  def reachable(distance: Double) = distance > 800 && distance < 2000
  def visitPoint(drone: Drone, p: InterestingPoint) =
    if (euclidean((drone.x, drone.y), (p.x, p.y)) < 50) (points - p, visitedPoints + p)
    else (points, visitedPoints)

  def nextClosestPoint(drone: Drone, distanceMap: Map[(Drone, InterestingPoint), Double]) = points.minByOption(p => distanceMap((drone, p)))
  def toMove(point: InterestingPoint, drone: Drone) = new Move(true, point.x, point.y, drone.battery > 5)
  def addDroneCreature(drone: Drone, creatureId: Int, radar: String): Unit = {
    val pair = (creatureId, radarMap(radar))
    drone.creatures = drone.creatures + pair
  }
  def resurfacePointOpt(drone: Drone) = if (prevScannedByMe < creatures.count(_.scannedByMe)) {
    Console.err.println(s"Need resurface! prevScannedByMe=$prevScannedByMe scannedCreatures=${creatures.count(_.scannedByMe)}")
    Some(InterestingPoint(drone.x, 499))
  } else {
    Console.err.println(s"No need resurface. prevScannedByMe=$prevScannedByMe scannedCreatures=${creatures.count(_.scannedByMe)}")
    None
  }
  def resurface(drone: Drone, score: Int, forMe: Boolean) {
    if (drone.y < 500) {
      if (forMe) {
        val byMeNum = creatures.count(_.scannedByMe)
        Console.err.println(s"RESURFACED! prevScannedByMe=$prevScannedByMe scannedCreatures=$byMeNum")
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

    val creatureDroneDistanceMap = (for (creature <- creatures; drone <- myDrones) yield {
      val distance = euclidean((drone.x, drone.y), (creature.x, creature.y))
      ((drone, creature), distance)
    }).toMap

    val dronePointsDistanceMap = (for (point <- points; drone <- myDrones) yield {
      val distance = euclidean((drone.x, drone.y), (point.x, point.y))
      ((drone, point), distance)
    }).toMap

    val interestingCreatures = creatures.filter(creature => creature.visible && !creature.scannedByMe)
//    // Console.err.println(s"Interesting creatures size=${interestingCreatures.length}")
//    // Console.err.println(s"Visited points=${visitedPoints}")







    myDrones.foreach(drone => {
     // Console.err.println(s"Scanned=${drone.scannedCreatures}")

      points.foreach(point => {
        val (newPoints, newVisitedPoints) = visitPoint(drone, point)
        points = newPoints
        visitedPoints = newVisitedPoints
      })

      val closestCreature = interestingCreatures.minByOption(creature => creatureDroneDistanceMap((drone, creature)))
      drone.lightOn = interestingCreatures.exists(creature => reachable(creatureDroneDistanceMap((drone, creature))))
      if (drone.lightOn) {
//        // Console.err.println(s"Dron-${drone.id} sees interesting creature! Drone battery=${drone.battery}")
      }
      val move = closestCreature.map(creature => {
          new Move(true, creature.x, creature.y, drone.battery > 5 && drone.lightOn)
        }).orElse(resurfacePointOpt(drone).map(point => toMove(point, drone)))
          .orElse(nextClosestPoint(drone, dronePointsDistanceMap).map(point => toMove(point, drone)))
          .orElse({
            // Console.err.println("It should not happen")
            Some(new Move(true, rand.nextInt(10000), rand.nextInt(10000), drone.battery > 5))
        })
        .get

      println(s"${move.print} ::drone ${drone.id} <${drone.x},${drone.y}> moves. Battery=${drone.battery}")

      resurface(drone, myScore, true)
    })
  }
}