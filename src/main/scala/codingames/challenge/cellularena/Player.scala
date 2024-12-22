package codingames.challenge.cellularena

import math._
import scala.annotation.tailrec
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
    def point: Point = (x, y)
  }
  case class Wall(x: Int, y: Int) extends Tile
  case class Organ(organId: Int, x: Int, y: Int, orgType: OrganType, orgDir: OrganDir, organParentId: Int, organRootId: Int, age: Int) extends Tile {
    def grow(x: Int, y: Int) = s"GROW $organId $x $y BASIC"
  }
  case class Protein(x: Int, y: Int, var harvested: Boolean = false) extends Tile
  case class Dao(from: Point, target: Point, direction: OrganDir)

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

  class Graph(amount: Int) {
    val adjacent = (for (_ <- 0 until amount) yield Set[Int]()).toArray
    var nodes = Map.empty[Int, Set[Int]]

    def addEdge(v: Int, w: Int) {
      adjacent(v) = adjacent(v) + w
      adjacent(w) = adjacent(w) + v
    }

    def cutEdge(v: Int, w: Int) {
      adjacent(v) = adjacent(v) - w
      adjacent(w) = adjacent(w) - v
    }

    def bfs(s: Int, marked: Array[Boolean] = new Array[Boolean](amount)): (Array[Int], Array[Int]) = {
      val edgeTo = Array.fill[Int](amount)(Int.MaxValue)
      val distTo = Array.fill[Int](amount)(Int.MaxValue)
      val q = mutable.Queue[Int]()
      var i = 0

      q.enqueue(s)
      marked(s) = true
      distTo(s) = 0
      while (q.nonEmpty) {
        val v = q.dequeue
        i = i + 1
        adjacent(v).filterNot(marked).foreach(
          w => {
            q.enqueue(w)
            marked(w) = true
            edgeTo(w) = v
            distTo(w) = distTo(v) + 1
          }
        )
      }
      (edgeTo, distTo)
    }
  }

// ---------------------------------------------------------------------------------------------------------------------
  def toMatrix(number: Int): (Int, Int) = (number % width, number / width)
  implicit def toNumber(point: (Int, Int)): Int = point._2 * width + point._1 % width
  implicit def stringToOrganDir(str: String): OrganDir = str match {
    case "N" => N
    case "W" => W
    case "S" => S
    case "E" => E
  }

  @tailrec
  def nextStep(from: Point, target: Point, edgeTo: Array[Int], distTo: Array[Int]): Option[Point] =
    if (distTo(target) > 10000) None else {
      val nextP = toMatrix(edgeTo(target))
      if (distTo(nextP) == 1) Some(nextP) else nextStep(from, nextP, edgeTo, distTo)
    }
  @tailrec
  def anyMove(candidateOpt: Option[Organ]): Option[Point] =
    if (candidateOpt.isEmpty) None else {
      val candidate = candidateOpt.get
      val out = adj(candidate.point)(air).headOption
      if (out.isDefined) out else anyMove(myOrganMap.get(candidate.organParentId))
    }
  def wallFree(p: Point) = !walls.contains(p)
  def myOrganFree(p: Point) = !myOrganSet.contains(p)
  def onlyOppOrgan(p: Point) = oppOrganSet.contains(p)
  def oppOrganFree(p: Point) = !onlyOppOrgan(p)
  def air(p: Point) = wallFree(p) && myOrganFree(p) && oppOrganFree(p)
  def inGraph(p: Point)(implicit filterFun: Point => Boolean = _ => true) = onlyOppOrgan(p) && filterFun(p)
  def withHarvest(xy: Point) = !proteinMap.get((xy._1, xy._2)).exists(_.harvested)
  def withTentacles(p: Point, myB: Int, myC: Int) = oppOrganFree(p) || (myB > 0 && myC > 0)
  def inGraphWithHarvest(xy: Point) = inGraph(xy)(withHarvest)
  def inGraphWithTentacles(xy: Point) = inGraph(xy)(withHarvest)

  var graphFilter: Point => Boolean = _

  def adjWithDirection(organ: Point)(implicit filterFun: Point => Boolean = _ => true) = {
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
        !wallsSet.contains((xy._1, xy._2)) &&
        filterFun((xy._1, xy._2))
    }
  }

  def adj(organ: Point)(implicit filterFun: Point => Boolean = _ => true) = {
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
      !wallsSet.contains(xy) &&
      filterFun(xy)
    }
  }

  def clean {
    walls = List.empty
    myOrgan = List.empty
    oppOrgan = List.empty
    proteins = List.empty
  }

  def harvest(organ: Organ)(implicit harvestFilter: => Boolean): Option[Dao] = {
    if (harvestFilter)
      adj(organ.point)(air)
        .filter(p => adj(p)(air).exists(p => proteinMap.get(p).exists(!_.harvested)))
        .map(moveCandidate => adjWithDirection(moveCandidate)
          .find(pd => proteinSet.contains((pd._1, pd._2)))
          .map(proteinInfo => Dao(moveCandidate, (proteinInfo._1, proteinInfo._2), proteinInfo._3)))
        .headOption.flatten
    else None
  }

  def tentacle(organ: Organ)(implicit tentacleFilter: => Boolean): Option[Dao] = {
    if (tentacleFilter) {
      adj(organ.point)(air)
        .map(r => (r, adjWithDirection(r)(onlyOppOrgan)))
        .filter(_._2.nonEmpty)
        .map(data => Dao(data._1, (data._2.head._1, data._2.head._2), data._2.head._3))
        .headOption
    } else None
  }

  def entry(entityCount: Int, step: Int) {
    for(_ <- 0 until entityCount) {
      // y: grid coordinate
      // _type: WALL, ROOT, BASIC, TENTACLE, HARVESTER, SPORER, A, B, C, D
      // owner: 1 if your organ, 0 if enemy organ, -1 if neither
      // organId: id of this entity if it's an organ, 0 otherwise
      // organDir: N,E,S,W or X if not an organ
      val Array(_x, _y, _type, _owner, _organId, organDir, _organParentId, _organRootId) = readLine split " "
      if (debug) Console.err.println(s"${_x} ${_y} ${_type} ${_owner} ${_organId} $organDir ${_organParentId} ${_organRootId}")

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

  }
    // ---------------------------------------------------------------------------------------------------------------------

// -------- game loop ------------------------------
  for (step <- LazyList.from(0).takeWhile(_ < 100)) {
    val entityCount = readLine.toInt
    if (debug) Console.err.println(s"$entityCount")
    clean
    entry(entityCount, step)

    myOrganMap = myOrgan.map(organ => (organ.organId, organ)).toMap
    oppOrganMap = oppOrgan.map(organ => (organ.organId, organ)).toMap
    myOrganSet = myOrgan.map(organ => organ.point).toSet
    oppOrganSet = oppOrgan.map(organ => organ.point).toSet
    proteinMap = proteins.map(protein => (protein.point, protein)).toMap
    proteinSet = proteinMap.keySet

    val Array(myA, myB, myC, myD) = (readLine split " ").withFilter(_ != "").map (_.toInt)
    if (debug) Console.err.println(s"$myA $myB $myC $myD")

    val Array(oppA, oppB, oppC, oppD) = (readLine split " ").filter(_ != "").map (_.toInt)
    if (debug) Console.err.println(s"$oppA $oppB $oppC $oppD")

    val requiredActionsCount = readLine.toInt // your number of organisms, output an action for each one in any order
    if (debug) Console.err.println(s"$requiredActionsCount")

    graphFilter = p => inGraphWithHarvest(p) && withTentacles(p, myB, myC)

    val graph = new Graph(width * height)
    for { x <- 0 until width;
          y <- 0 until height;
          if graphFilter((x, y))
          } {
      adj((x, y))(graphFilter).foreach(a => graph.addEdge(a, (x, y)))
    }

    for(_ <- 0 until requiredActionsCount) {
      growCandidate = myOrgan.maxBy(_.age)

      adj(growCandidate.point)(graphFilter).foreach(graph.addEdge(growCandidate.point, _))
      val  (edgeTo, distTo) = graph.bfs(growCandidate.point)

      val command = harvest(growCandidate)(myC > 0 && myD > 0)
        .map(hrv => {
          val protein = proteinMap(hrv.target)
          protein.harvested = true
          s"GROW ${growCandidate.organId} ${hrv.from._1} ${hrv.from._2} HARVESTER ${hrv.direction}"})
        .orElse {
          tentacle(growCandidate)(myB > 0 && myC > 0)
            .map(dao => s"GROW ${growCandidate.organId} ${dao.target._1} ${dao.target._2} TENTACLE ${dao.direction}")
        }
        .orElse {
          val closestProtein = proteins.filterNot(_.harvested).minByOption(protein => distTo(protein.point))
          closestProtein
            .flatMap(protein => nextStep(growCandidate.point, protein.point, edgeTo, distTo)
                .map(nextXY => s"GROW ${growCandidate.organId} ${nextXY._1} ${nextXY._2} BASIC"))
        }
        .orElse {
          nextStep(growCandidate.point, oppOrgan.head.point, edgeTo, distTo)
          .map(p => s"GROW ${growCandidate.organId} ${p._1} ${p._2} BASIC")
        }
        .orElse {
          anyMove(Some(growCandidate))
          .map(p => s"GROW ${growCandidate.organId} ${p._1} ${p._2} BASIC")
        }
        .getOrElse("WAIT")

      println(command)

    }
  }
}
