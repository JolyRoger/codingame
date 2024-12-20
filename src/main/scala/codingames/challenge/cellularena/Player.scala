package codingames.challenge.cellularena

import math._
import scala.collection.mutable
import scala.util._
import scala.io.StdIn._
import scala.io.Source
import scala.language.implicitConversions

/**
 * Grow and multiply your organisms to end up larger than your opponent.
 **/
object Player extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
  val filename = "resources/organisms/1.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines
  def readInt = if (data.hasNext) data.next.toInt else { System.exit(0); -1 }
  def readLine = if (data.hasNext) data.next else { System.exit(0); "" }
//----------------------------------------------------------------------------------------------------------------------

  val debug = false

  type Point = (Int, Int)
  object OrganDir extends Enumeration {
    type OrganDir = Value
    val N, W, S, E = Value
  }

  object OrganType extends Enumeration {
    type OrganType = Value
    val ROOT, BASIC, HARVESTER, TENTACLE, SPORER = Value
  }

  import OrganType._
  import OrganDir._

  trait Tile {
    def x: Int
    def y: Int
  }
  case class Wall(x: Int, y: Int) extends Tile
  case class Organ(organId: Int, x: Int, y: Int, orgType: OrganType, orgDir: OrganDir, organParentId: Int, organRootId: Int, age: Int) extends Tile {
    def grow(x: Int, y: Int) = s"GROW $organId $x $y BASIC"
  }
  case class Protein(x: Int, y: Int, var harvested: Boolean = false) extends Tile
  case class Harvest(candidate: Point, protein: Point, direction: OrganDir)

  var walls = List.empty[Wall]
  var myOrgan = List.empty[Organ]
  var oppOrgan = List.empty[Organ]
  var proteins = List.empty[Protein]

  var myOrganMap = Map.empty[Int, Organ]
  var oppOrganMap = Map.empty[Int, Organ]
  var proteinMap = Map.empty[Point, Protein]

  lazy val wallsSet = walls.map(wall => (wall.x, wall.y)).toSet
  var myOrganSet = myOrgan.map(organ => (organ.x, organ.y)).toSet
  var oppOrganSet = oppOrgan.map(organ => (organ.x, organ.y)).toSet
  var proteinSet = proteins.map(protein => (protein.x, protein.y)).toSet

  var growCandidate: Organ = _

  // width: columns in the game grid
  // height: rows in the game grid
  val Array(width, height) = (readLine split " ").withFilter(_ != "").map (_.toInt)
  if (debug) Console.err.println(s"$width $height")


// ---------------------------------------------------------------------------------------------------------------------
  def toMatrix(number: Int): (Int, Int) = (number % width, number / width)
  implicit def toNumber(point: (Int, Int)): Int = point._2 * width + point._1 % width
  implicit def stringToOrganDir(str: String): OrganDir = str match {
    case "N" => N
    case "W" => W
    case "S" => S
    case "E" => E
  }

  def euclidean(a: Point, b: Point): Double = hypot(b._1 - a._1, b._2 - a._2)

  def adjWithDirection(organ: Point) = {
    Set(
      (organ._1, organ._2 + 1, S),
      (organ._1, organ._2 - 1, N),
      (organ._1 + 1, organ._2, E),
      (organ._1 - 1, organ._2, W)
    ).filter { xy =>
      xy._1 >= 0 &&
        xy._1 < width &&
        xy._2 >= 0 &&
        xy._2 < height &&
        !wallsSet.contains((xy._1, xy._2))
    }
  }

  def adj(organ: Point) = {
    Set(
      (organ._1, organ._2 + 1),
      (organ._1, organ._2 - 1),
      (organ._1 + 1, organ._2),
      (organ._1 - 1, organ._2)
    ).filter { xy =>
      xy._1 >= 0 &&
      xy._1 < width &&
      xy._2 >= 0 &&
      xy._2 < height &&
      !wallsSet.contains(xy)
    }
  }

  def clean {
    walls = List.empty
    myOrgan = List.empty
    oppOrgan = List.empty
    proteins = List.empty
  }

  def harvest(organ: Organ) =
    adj((organ.x, organ.y))
      .filter(p => adj(p).exists(p => proteinSet.contains((p._1, p._2))))
      .map(moveCandidate => adjWithDirection(moveCandidate)
        .find(pd => proteinSet.contains((pd._1, pd._2)))
        .map(proteinInfo => Harvest(moveCandidate, (proteinInfo._1, proteinInfo._2), proteinInfo._3)))
      .headOption.flatten

// ---------------------------------------------------------------------------------------------------------------------

// -------- game loop ------------------------------
  for (step <- LazyList.from(0).takeWhile(_ < 100)) {
    val entityCount = readLine.toInt
    if (debug) Console.err.println(s"$entityCount")
    clean

    for(_ <- 0 until entityCount) {
      // y: grid coordinate
      // _type: WALL, ROOT, BASIC, TENTACLE, HARVESTER, SPORER, A, B, C, D
      // owner: 1 if your organ, 0 if enemy organ, -1 if neither
      // organId: id of this entity if it's an organ, 0 otherwise
      // organDir: N,E,S,W or X if not an organ
      val Array(_x, _y, _type, _owner, _organId, organDir, _organParentId, _organRootId) = readLine split " "
      if (debug) Console.err.println(s"${_x} ${_y} ${_type} ${_owner} ${_organId} ${organDir} ${_organParentId} ${_organRootId}")

      val x = _x.toInt
      val y = _y.toInt
      val owner = _owner.toInt
      val organId = _organId.toInt
      val organParentId = _organParentId.toInt
      val organRootId = _organRootId.toInt

      if (owner == -1) {
        _type match {
          case "WALL" =>
            val wall = Wall(x, y)
            walls = wall :: walls
          case "A" =>
            val harvested = proteinMap.get((x, y)).exists(_.harvested)
            val protein = Protein(x, y, harvested)
            proteins = protein :: proteins
        }
      } else {
        val allOrgans = myOrganMap ++ oppOrganMap
        val organAge = allOrgans.get(organId).map(_.age).getOrElse(step)
        val organ = Organ(organId, x, y, OrganType.withName(_type), organDir, organParentId, organRootId, organAge)
        if (owner == 1) myOrgan ::= organ else oppOrgan ::= organ
      }
    }

    myOrganSet = myOrgan.map(organ => (organ.x, organ.y)).toSet
    oppOrganSet = oppOrgan.map(organ => (organ.x, organ.y)).toSet
    proteinMap = proteins.map(protein => ((protein.x, protein.y), protein)).toMap
    proteinSet = proteinMap.keySet

    // myD: your protein stock
    val Array(myA, myB, myC, myD) = (readLine split " ").withFilter(_ != "").map (_.toInt)
    if (debug) Console.err.println(s"$myA $myB $myC $myD")

    // oppD: opponent's protein stock
    val Array(oppA, oppB, oppC, oppD) = (readLine split " ").filter(_ != "").map (_.toInt)
    if (debug) Console.err.println(s"$oppA $oppB $oppC $oppD")

    val requiredActionsCount = readLine.toInt // your number of organisms, output an action for each one in any order
    if (debug) Console.err.println(s"$requiredActionsCount")

    for(_ <- 0 until requiredActionsCount) {
      // Write an action using println
      // To debug: Console.err.println("Debug messages...")
      growCandidate = myOrgan.maxBy(_.age)

      val command = harvest(growCandidate)
        .filter(_ => myC > 0 && myD > 0)
        .map(hrv => {
          val protein = proteinMap(hrv.protein)
          protein.harvested = true
          s"GROW ${growCandidate.organId} ${hrv.candidate._1} ${hrv.candidate._2} HARVESTER ${hrv.direction}"
        })
        .getOrElse {
          val closestProtein = proteins.minByOption(protein => euclidean((protein.x, protein.y), (growCandidate.x, growCandidate.y)))
          s"${closestProtein
            .map(protein => s"GROW ${growCandidate.organId} ${protein.x} ${protein.y} BASIC")
            .getOrElse(s"GROW ${growCandidate.organId} ${oppOrgan.head.x} ${oppOrgan.head.y} BASIC")}"
        }

      println(command)

    }
  }
}
