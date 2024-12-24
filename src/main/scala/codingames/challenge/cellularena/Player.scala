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
    val ROOT, BASIC, HARVESTER, TENTACLE, SPORER, UNKNOWN = Value
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
  case class Dao(organId: Int, candidate: Point, target: Point, direction: OrganDir)

  var typeA: Int = _
  var typeB: Int = _
  var typeC: Int = _
  var typeD: Int = _

  var walls = List.empty[Wall]
  var myOrgan = List.empty[Organ]
  var oppOrgan = List.empty[Organ]
  var proteins = List.empty[Protein]
  var myOrgans = Map.empty[Int, List[Organ]]

  var myOrganMap = Map.empty[Int, Organ]
  var oppOrganMap = Map.empty[Int, Organ]
  var myOrganMapCoord = Map.empty[Point, Int]
  var oppOrganMapCoord = Map.empty[Point, Int]
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
  def selectOrganType: OrganType =
    if (typeA > 0) BASIC else
    if (typeB > 0 && typeC > 0) TENTACLE else
    if (typeB > 0 && typeD > 0) SPORER else
    if (typeC > 0 && typeD > 0) HARVESTER else UNKNOWN

  def decrementProtein(organType: OrganType) {
    organType match {
      case BASIC => typeA -= 1
      case TENTACLE => typeB -= 1; typeC -= 1
      case SPORER => typeB -= 1; typeD-= 1
      case HARVESTER => typeC -= 1; typeD-= 1
      case ROOT => typeA -= 1; typeB-= 1; typeC -= 1; typeD-= 1
    }
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

  def visible(point: Point, target: Point) = point._1 == target._1 || point._2 == target._2

  def visibleToDao(organ: Organ, point: Point, target: Point) = {
      if (point._2 < target._2) Dao(organ.organId, point, target, N) else
      if (point._2 > target._2) Dao(organ.organId, point, target, S) else
      if (point._1 < target._1) Dao(organ.organId, point, target, E) else
      if (point._1 > target._1) Dao(organ.organId, point, target, W) else
          throw IllegalArgumentException
  }

  def proteinHarvestVisible(organ: Organ, adjPoint: Point): Set[Dao] = {
    proteinSet.flatMap {
      p => {
        val res = adjHarvest(p)(air).map(candidate => visibleToDao(proteinDirectVisible(organ, candidate)))
        res
      }
    }
  }

  def proteinDirectVisible(organ: Organ, adjPoint: Point): Set[Dao] = {
    proteinSet.withFilter(visible(adjPoint, _))
      .map(visibleToDao(organ, adjPoint, _))
  }

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

  def adjHarvest(p: Point)(implicit filterFun: Point => Boolean = _ => true) = {
    Set(
      (p._1, p._2 + 2),
      (p._1, p._2 - 2),
      (p._1 + 2, p._2),
      (p._1 - 2, p._2),
      (p._1 + 1, p._2 + 1),
      (p._1 + 1, p._2 - 1),
      (p._1 - 1, p._2 + 1),
      (p._1 - 1, p._2 - 1)
    ).filter { xy =>
      xy._1 >= 0 &&
        xy._1 < width &&
        xy._2 >= 0 &&
        xy._2 < height &&
        filterFun(xy)
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

  def sporeShoot: Option[Dao] = {
    if (typeA > 0 && typeB > 0 && typeC > 0 && typeD > 0) {
      myOrgan.withFilter(_.orgType == SPORER).flatMap { organ =>
        proteinDirectVisible(organ, organ.point)
      }.headOption
    } else None
  }

  def sporer: Option[Dao] = {
    if (typeA > 0 && typeB > 1 && typeC > 0 && typeD > 1) {
      myOrgan.flatMap { organ =>
        adj(organ.point)(air).flatMap(p => proteinHarvestVisible(organ, p))
      }.headOption
    } else None
  }

  def harvest(organ: Organ)(implicit harvestFilter: => Boolean): Option[Dao] = {
    if (harvestFilter)
      adj(organ.point)(air)
        .filter(p => adj(p)(air).exists(p => proteinMap.get(p).exists(!_.harvested)))
        .map(moveCandidate => adjWithDirection(moveCandidate)
          .find(pd => proteinSet.contains((pd._1, pd._2)))
          .map(proteinInfo => Dao(organ.organId, moveCandidate, (proteinInfo._1, proteinInfo._2), proteinInfo._3)))
        .headOption.flatten
    else None
  }

  def tentacle(organ: Organ)(implicit tentacleFilter: => Boolean): Option[Dao] = {
    if (tentacleFilter) {
      adj(organ.point)(air)
        .map(r => (r, adjWithDirection(r)(onlyOppOrgan)))
        .filter(_._2.nonEmpty)
        .map(data => Dao(organ.organId, data._1, (data._2.head._1, data._2.head._2), data._2.head._3))
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

  for (step <- LazyList.from(0).takeWhile(_ < 100)) {
    val entityCount = readLine.toInt
    if (debug) Console.err.println(s"$entityCount")
    clean
    entry(entityCount, step)

    myOrgans = myOrgan.groupBy(_.organRootId)
    myOrganMap = myOrgan.map(organ => (organ.organId, organ)).toMap
    oppOrganMap = oppOrgan.map(organ => (organ.organId, organ)).toMap
    myOrganMapCoord = myOrgan.map(organ => (organ.point, organ.organId)).toMap
    oppOrganMapCoord = oppOrgan.map(organ => (organ.point, organ.organId)).toMap
    myOrganSet = myOrgan.map(organ => organ.point).toSet
    oppOrganSet = oppOrgan.map(organ => organ.point).toSet
    proteinMap = proteins.map(protein => (protein.point, protein)).toMap
    proteinSet = proteinMap.keySet

    val Array(myA, myB, myC, myD) = (readLine split " ").withFilter(_ != "").map (_.toInt)
    if (debug) Console.err.println(s"$myA $myB $myC $myD")
    typeA = myA
    typeB = myB
    typeC = myC
    typeD = myD

    val Array(oppA, oppB, oppC, oppD) = (readLine split " ").filter(_ != "").map (_.toInt)
    if (debug) Console.err.println(s"$oppA $oppB $oppC $oppD")

    val requiredActionsCount = readLine.toInt // your number of organisms, output an action for each one in any order
    if (debug) Console.err.println(s"$requiredActionsCount")

    graphFilter = p => inGraphWithHarvest(p) && withTentacles(p, typeB, typeC)

    val graph = new Graph(width * height)
    for { x <- 0 until width;
          y <- 0 until height;
          if graphFilter((x, y))
          } {
      adj((x, y))(graphFilter).foreach(a => graph.addEdge(a, (x, y)))
    }

    val myOrganIterator = myOrgans.keySet.iterator

    for(_ <- 0 until requiredActionsCount) {
      val key = myOrganIterator.next()
      val organs = myOrgans(key)
      growCandidate = organs.maxBy(_.age)

      adj(growCandidate.point)(graphFilter).foreach(graph.addEdge(growCandidate.point, _))
      val  (edgeTo, distTo) = graph.bfs(growCandidate.point)

      val command = sporeShoot.map(dao => {
          typeA -= 1; typeB -= 1; typeC -= 1; typeD -= 1
          s"SPORE ${dao.organId} ${dao.target._1} ${dao.target._2}"
        })
        .orElse {
          harvest(growCandidate)(typeC > 0 && typeD > 0)
          .map(hrv => {
            val protein = proteinMap(hrv.target)
            protein.harvested = true
            typeC -= 1; typeD -= 1
            s"GROW ${growCandidate.organId} ${hrv.candidate._1} ${hrv.candidate._2} $HARVESTER ${hrv.direction}"
          })
        }
        .orElse {
          sporer.map(dao => {
            typeB -= 1; typeD -= 1
            s"GROW ${dao.organId} ${dao.candidate._1} ${dao.candidate._2} $SPORER ${dao.direction}"
          })
        }
        .orElse {
          tentacle(growCandidate)(typeB > 0 && typeC > 0)
            .map(dao => {
              typeB -= 1; typeC -= 1
              s"GROW ${growCandidate.organId} ${dao.target._1} ${dao.target._2} $TENTACLE ${dao.direction}"
            })
        }
        .orElse {
          val closestProtein = proteins.filterNot(_.harvested).minByOption(protein => distTo(protein.point))
          closestProtein
            .flatMap(protein => nextStep(growCandidate.point, protein.point, edgeTo, distTo)
                .map(nextXY => {
                  typeA -= 1
                  s"GROW ${growCandidate.organId} ${nextXY._1} ${nextXY._2} $BASIC"
                }))
        }
        .orElse {
          nextStep(growCandidate.point, oppOrgan.head.point, edgeTo, distTo)
          .map(p => {
            typeA -= 1
            s"GROW ${growCandidate.organId} ${p._1} ${p._2} $BASIC"
          })
        }
        .orElse {
          anyMove(Some(growCandidate))
            .filter(_ => typeA > 0 || (typeB > 0 && typeC > 0) || (typeC > 0 && typeD > 0) || (typeB > 0 && typeD > 0))
            .map(p => {
              val organType = selectOrganType
              decrementProtein(organType)
              s"GROW ${growCandidate.organId} ${p._1} ${p._2} $organType"
            })
        }
        .getOrElse("WAIT")

      println(command)

    }
//    println(s"--------------------------------------------------------------------")
  }
}
