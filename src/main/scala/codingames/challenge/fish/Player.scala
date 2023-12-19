package codingames.challenge.fish

import math._
import scala.io.Source
import scala.util._
import scala.io.StdIn._

/**
 * Score points by scanning valuable fish faster than your opponent.
 **/
object Player extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
  val filename = "resources/fish/1.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines
  def readInt = if (data.hasNext) data.next.toInt else { System.exit(0); -1 }
  def readLine = if (data.hasNext) data.next else { System.exit(0); "" }
//----------------------------------------------------------------------------------------------------------------------

  class Creature(val id: Int, val color: Int, val ctype: Int) {
    var creatureX: Int = -1
    var creatureY: Int = -1
    var creatureVx: Int = -1
    var creatureVy: Int = -1
    var visible = false
  }
  case class Drone(droneId: Int, droneX: Int, droneY: Int, emergency: Int, battery: Int)

  class Move(var isMove: Boolean, var xpos: Int, var ypos: Int, var isLightOn: Boolean) {
    def print = {
      val move = s"${if (isMove) "MOVE" else "WAIT"}"
      val xy = s" ${xpos} ${ypos}"
      val coord = s"${if (isMove) xy else ""}"
      val light = s"${if (isLightOn) " 1" else " 0"}"
      s"$move$coord$light"
    }
  }

  val rand = new Random(System.currentTimeMillis)
  val creatureCount = readLine.toInt
  Console.err.println(s"$creatureCount")

  val creatures = for (_ <- 0 until creatureCount) yield {
    val Array(creatureId, color, _type) = (readLine split " ").filter(_ != "").map(_.toInt)
    Console.err.println(s"$creatureId $color ${_type}")
    new Creature(creatureId, color, _type)
  }
  val creaturesMap = creatures.map(creature => (creature.id, creature)).toMap

  // game loop
  while (true) {
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
    val myDrones = for (i <- 0 until myDroneCount) yield {
      val Array(droneId, droneX, droneY, emergency, battery) = (readLine split " ").filter(_ != "").map(_.toInt)
      Console.err.println(s"$droneId $droneX $droneY $emergency $battery")
      Drone(droneId, droneX, droneY, emergency, battery)
    }
    val foeDroneCount = readLine.toInt
    Console.err.println(s"$foeDroneCount")
    val foeDrones = for (i <- 0 until foeDroneCount) yield {
      val Array(droneId, droneX, droneY, emergency, battery) = (readLine split " ").filter(_ != "").map(_.toInt)
      Console.err.println(s"$droneId $droneX $droneY $emergency $battery")
      Drone(droneId, droneX, droneY, emergency, battery)
    }
    val droneScanCount = readLine.toInt
    Console.err.println(s"$droneScanCount")
    for (i <- 0 until droneScanCount) {
      val Array(droneId, creatureId) = (readLine split " ").filter(_ != "").map(_.toInt)
      Console.err.println(s"$droneId $creatureId")
    }
    val visibleCreatureCount = readLine.toInt
    Console.err.println(s"$visibleCreatureCount")

    creatures.foreach(creature => creature.visible = false)
    for (i <- 0 until visibleCreatureCount) {
      val Array(creatureId, creatureX, creatureY, creatureVx, creatureVy) = (readLine split " ").filter(_ != "").map(_.toInt)
      Console.err.println(s"$creatureId $creatureX $creatureY $creatureVx $creatureVy")
      val creature = creaturesMap(creatureId)
      creature.creatureX = creatureX
      creature.creatureY = creatureY
      creature.creatureVx = creatureVx
      creature.creatureVy = creatureVy
      creature.visible = true
    }
    val radarBlipCount = readLine.toInt
    Console.err.println(s"$radarBlipCount")
    for (i <- 0 until radarBlipCount) {
      val Array(_droneId, _creatureId, radar) = readLine split " "
      Console.err.println(s"${_droneId} ${_creatureId} $radar")
      val droneId = _droneId.toInt
      val creatureId = _creatureId.toInt
    }


    myDrones.foreach(drone => {
      var nextMove = new Move(true, rand.nextInt(10000), rand.nextInt(10000), drone.battery > 5)
      println(s"${nextMove.print} ::drone ${drone.droneId} moves. Battery=${drone.battery}")
    })
  }
}