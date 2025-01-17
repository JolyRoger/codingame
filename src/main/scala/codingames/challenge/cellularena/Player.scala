package codingames.challenge.cellularena

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util._
import scala.io.StdIn._
import scala.io.Source
import scala.language.implicitConversions

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

  object ProteinType extends Enumeration {
    type ProteinType = Value
    val A, B, C, D = Value
  }

  object OrganType extends Enumeration {
    type OrganType = Value
    val ROOT, BASIC, HARVESTER, TENTACLE, SPORER, UNKNOWN = Value
  }

  import OrganType._
  import ProteinType._
  import OrganDir._

  trait Tile {
    def x: Int
    def y: Int
    def point: Point = (x, y)
  }
  case class Wall(x: Int, y: Int) extends Tile
  case class Organ(organId: Int, x: Int, y: Int, orgType: OrganType, orgDir: OrganDir, organParentId: Int,
                   organRootId: Int) extends Tile
  case class Protein(x: Int, y: Int, ptype: ProteinType, var harvested: Boolean = false) extends Tile
  case class Dao(organId: Int, candidate: Point, target: Point, direction: OrganDir) {
    lazy val dist = Math.max(Math.abs(candidate._1 - target._1), Math.abs(candidate._2 - target._2))
  }

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

  var wallsSet = walls.map(wall => (wall.x, wall.y)).toSet
  var myOrganSet = myOrgan.map(organ => (organ.x, organ.y)).toSet
  var oppOrganSet = oppOrgan.map(organ => (organ.x, organ.y)).toSet
  var oppTentacled = Set.empty[Point]
  var growCandidates = Set.empty[Point]
  var proteinSet = proteins.map(protein => (protein.x, protein.y)).toSet

  var growCandidate: Organ = _
  var oppOrganHash: Int = _
  var myOrganHash: Int = _
  var wallsHash: Int = _
  var ryba: Boolean = false
  var onLeft: Boolean = false

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
//  def euclidean(a: Point, b: Point): Double = hypot(b._1 - a._1, b._2 - a._2)
  def toMatrix(number: Int): (Int, Int) = (number % width, number / width)
  implicit def toNumber(point: (Int, Int)): Int = point._2 * width + point._1 % width
  implicit def stringToOrganDir(str: String): OrganDir = str match {
    case "N" => N
    case "W" => W
    case "S" => S
    case "E" => E
  }
  implicit def stringToProteinType(str: String): ProteinType = str match {
    case "A" => A
    case "B" => B
    case "C" => C
    case "D" => D
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
      case UNKNOWN => Console.err.println("Should not happen!")
    }
  }

  @tailrec
  def nextStep(from: Point, target: Point, edgeTo: Array[Int], distTo: Array[Int]): Option[Point] =
    if (distTo(target) > 10000) None else {
      val nextP = toMatrix(edgeTo(target))
      if (distTo(nextP) == 1) Some(nextP) else nextStep(from, nextP, edgeTo, distTo)
    }

  @tailrec
  def anyMove(candidateOpt: Option[Organ]): Option[Dao]  =
    if (candidateOpt.isEmpty) None else {
      val candidate = candidateOpt.get
      val out = adj(candidate.point)(airNoHarvest).maxByOption(p => if (onLeft) p._1 else -p._1)
      if (out.isDefined) Some(Dao(candidate.organId, out.get, out.get, E)) else
        anyMove(myOrganMap.get(candidate.organParentId))
    }

  def inside(p: Point) = p._1 >= 0 && p._1 < width && p._2 >= 0 && p._2 < height
  def wallFree(p: Point) = !wallsSet.contains(p)
  def myOrganFree(p: Point) = !myOrganSet.contains(p)
  def oppOrganFree(p: Point) = !onlyOppOrgan(p)
  def onlyOppOrgan(p: Point) = oppOrganSet.contains(p)
  def tentacleSafe(p: Point) = !oppTentacled.contains(p)
  def notForGrowStill(p: Point) = !growCandidates.contains(p)
  def air(p: Point) = wallFree(p) && myOrganFree(p) && oppOrganFree(p)
  def forGrow(p: Point) = wallFree(p) && myOrganFree(p) && oppOrganFree(p) && tentacleSafe(p) && notForGrowStill(p)
  def inGraph(p: Point)(implicit filterFun: Point => Boolean = _ => true) = onlyOppOrgan(p) && filterFun(p)
  def noHarvest(xy: Point) = !proteinMap.get((xy._1, xy._2)).exists(_.harvested)
  def airNoHarvest(p: Point) = forGrow(p) && noHarvest(p)
  def airDist2(from: Point, target: Point) = {
      if (from._1 == target._1 && from._2 == target._2 + 2) forGrow(from._1, target._2 + 1) else
      if (from._1 == target._1 && from._2 == target._2 - 2) forGrow(from._1, target._2 - 1) else
      if (from._2 == target._2 && from._1 == target._1 + 2) forGrow(from._1 - 1, target._2) else
      if (from._2 == target._2 && from._1 == target._1 - 2) forGrow(from._1 + 1, target._2) else
        forGrow(from._1, target._2) || forGrow(target._1, from._2)
  }
  def withTentacles(p: Point, myB: Int, myC: Int) = oppOrganFree(p) || (myB > 0 && myC > 0)
  def inGraphWithHarvest(xy: Point) = inGraph(xy)(noHarvest)

  var graphFilter: Point => Boolean = _

  def visible(point: Point, target: Point) = {
    (point != target) && {
      if (point._1 == target._1) {
        if (point._2 < target._2) (point._2 + 1 to target._2).forall(y => forGrow(point._1, y)) else
        if (point._2 > target._2) (target._2 until point._2).forall(y => forGrow(point._1, y)) else
          throw new IllegalArgumentException("Wrong y parameter")
      } else if (point._2 == target._2) {
        if (point._1 < target._1) (point._1 + 1 to target._1).forall(x => forGrow(x, point._2)) else
        if (point._1 > target._1) (target._1 until point._1).forall(x => forGrow(x, point._2)) else
          throw new IllegalArgumentException("Wrong x parameter")
      } else false
    }
  }

  def visibleToDao(organ: Organ, point: Point, target: Point) = {
      if (point._2 < target._2) Dao(organ.organId, point, target, S) else
      if (point._2 > target._2) Dao(organ.organId, point, target, N) else
      if (point._1 < target._1) Dao(organ.organId, point, target, E) else
      if (point._1 > target._1) Dao(organ.organId, point, target, W) else
          throw new IllegalArgumentException("Should not happen!")
  }

  def proteinSporerVisible(organ: Organ, adjPoint: Point): Set[Dao] = {
    proteinSet.withFilter(noHarvest).flatMap {
      p => adjHarvest(p)(target => forGrow(target) && airDist2(p, target))
            .withFilter(visible(adjPoint, _))
            .map(visibleToDao(organ, adjPoint, _))
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
    ).filter { xy => xy._1 >= 0 &&
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
    ).filter { xy => inside(xy) && filterFun(xy)
    }
  }

  def adj(organ: Point)(implicit filterFun: Point => Boolean = _ => true) = {
    Set(
      (organ._1, organ._2 + 1),
      (organ._1, organ._2 - 1),
      (organ._1 + 1, organ._2),
      (organ._1 - 1, organ._2)
    ).filter { xy =>
      inside(xy) &&
      !wallsSet.contains(xy) &&
      filterFun(xy)
    }
  }

  def clean {
    walls = List.empty
    myOrgan = List.empty
    oppOrgan = List.empty
    proteins = List.empty
    oppTentacled = Set.empty
    growCandidates = Set.empty
  }

  def sporeShoot(organs: List[Organ])(implicit sporeShootFilter: => Boolean): Option[Dao] = {
    if (sporeShootFilter) {
      organs.withFilter(_.orgType == SPORER).flatMap { organ =>
        proteinSporerVisible(organ, organ.point).filter(dao => dao.direction == organ.orgDir && dao.dist > 3)
      }.maxByOption(_.dist)
    } else None
  }

  def sporer(organ: Organ)(implicit sporerFilter: => Boolean): Option[Dao] = {
    if (sporerFilter) {
        for (point <- adj(organ.point)(airNoHarvest)) {
          val daos = proteinSporerVisible(organ, point).filter(_.dist > 3)
          if (daos.nonEmpty) return daos.maxByOption(_.dist)
        }
        None
    } else None
  }

  def harvest(organ: Organ)(implicit harvestFilter: => Boolean): Option[Dao] = {
    if (harvestFilter)
      adj(organ.point)(airNoHarvest)
        .filter(p => adj(p)(airNoHarvest).exists(p => proteinMap.get(p).exists(!_.harvested)))
        .map(moveCandidate => adjWithDirection(moveCandidate)
          .find(pd => proteinSet.contains((pd._1, pd._2)))
          .map(proteinInfo => Dao(organ.organId, moveCandidate, (proteinInfo._1, proteinInfo._2), proteinInfo._3)))
        .headOption.flatten
    else None
  }

  def tentacle(organs: List[Organ])(implicit tentacleFilter: => Boolean): Option[Dao] = {
    if (tentacleFilter) {
      for (organ <- organs.sortBy(_.organId)) {
        if (organ.organId == 193) Console.err.println(s"tentacled: ${oppTentacled.mkString(",")}")
        val candidate = adj(organ.point)(forGrow)
          .map(r => (r, adjWithDirection(r)(onlyOppOrgan)))
          .filter(_._2.nonEmpty)
          .map(data => Dao(organ.organId, data._1, (data._2.head._1, data._2.head._2), data._2.head._3))
          .headOption
        if (candidate.nonEmpty) return candidate
      }
      None
    } else None
  }

  def getCommand(organs: List[Organ], requiredActionsCount: Int, edgeTo: Array[Int], distTo: Array[Int])  =
    tentacle(organs)(typeB > 0 && typeC > 0)
      .map(dao => {
        typeB -= 1; typeC -= 1
        growCandidates += dao.candidate
        s"GROW ${dao.organId} ${dao.candidate._1} ${dao.candidate._2} $TENTACLE ${dao.direction}"
      }).orElse {
      harvest(growCandidate)(typeC > 0 && typeD > 0)
        .map(hrv => {
          val protein = proteinMap(hrv.target)
          protein.harvested = true
          typeC -= 1; typeD -= 1
          growCandidates += hrv.candidate
          s"GROW ${growCandidate.organId} ${hrv.candidate._1} ${hrv.candidate._2} $HARVESTER ${hrv.direction}"
      })}.orElse {
      sporeShoot(organs)(typeA > 0 && typeB > 0 && typeC > 0 && typeD > 0 && requiredActionsCount < 6).map(dao => {
        typeA -= 1; typeB -= 1; typeC -= 1; typeD -= 1
        growCandidates += dao.target
        s"SPORE ${dao.organId} ${dao.target._1} ${dao.target._2}"
      })}.orElse {
      sporer(growCandidate)(typeA > 0 && typeB > 1 && typeC > 0 && typeD > 1).map(dao => {
        typeB -= 1; typeD -= 1
        growCandidates += dao.candidate
        s"GROW ${dao.organId} ${dao.candidate._1} ${dao.candidate._2} $SPORER ${dao.direction}"
      })}.orElse {
      val closestProtein = proteins.filterNot(_.harvested).minByOption(protein => distTo(protein.point))
      closestProtein.flatMap(protein => nextStep(growCandidate.point, protein.point, edgeTo, distTo)
        .map(nextXY => {
          typeA -= 1
          growCandidates += nextXY
          s"GROW ${growCandidate.organId} ${nextXY._1} ${nextXY._2} $BASIC"
      }))}.orElse {
      nextStep(growCandidate.point, oppOrgan.head.point, edgeTo, distTo)
        .map(p => {
          typeA -= 1
          growCandidates += p
          s"GROW ${growCandidate.organId} ${p._1} ${p._2} $BASIC"
      })}.orElse {
      anyMove(Some(growCandidate))
        .filter(_ => typeA > 0 || (typeB > 0 && typeC > 0) || (typeC > 0 && typeD > 0) || (typeB > 0 && typeD > 0))
        .map(dao => {
          val organType = selectOrganType
          decrementProtein(organType)
          growCandidates += dao.candidate
          s"GROW ${dao.organId} ${dao.candidate._1} ${dao.candidate._2} $organType"
        })
      }
      .getOrElse("WAIT")

  def getRybaCommand(organs: List[Organ]) = {
    if (typeA > 0 || (typeB > 0 && typeC > 0) || (typeC > 0 && typeD > 0) || (typeB > 0 && typeD > 0)) {
      val candidateOpt = organs.find(organ => adj(organ.point)(forGrow).nonEmpty)
      candidateOpt.map(candidate => {
        val target = adj(candidate.point)(forGrow).head
        val organType = selectOrganType
        decrementProtein(organType)
        s"GROW ${candidate.organId} ${target._1} ${target._2} $organType"
      }).getOrElse("WAIT")
    } else "WAIT"
  }

  def createProtein(x: Int, y: Int, typeStr: String) = Protein(x, y, typeStr, proteinMap.get((x, y)).exists(_.harvested)) :: proteins

  def entry(entityCount: Int, step: Int) {
    for (_ <- 0 until entityCount) {
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
          case "A" => proteins = createProtein(x, y, "A")
          case "B" => proteins = createProtein(x, y, "B")
          case "C" => proteins = createProtein(x, y, "C")
          case "D" => proteins = createProtein(x, y, "D")
        }
      } else {
        val organ = Organ(organId, x, y, OrganType.withName(_type), organDir, organParentId, organRootId)
        if (owner == 1) myOrgan ::= organ else {
          oppOrgan ::= organ
          if (organ.orgType == TENTACLE) {
            val p = organ.orgDir match {
              case W => (x - 1, y)
              case S => (x, y + 1)
              case E => (x + 1, y)
              case N => (x, y - 1)
            }
            if (inside(p)) oppTentacled += p
          }
        }
      }
    }
    if (step == 0) onLeft = myOrgan.head.point._1 < oppOrgan.head.point._1
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
    wallsSet = walls.map(wall => (wall.x, wall.y)).toSet
    myOrganSet = myOrgan.map(organ => organ.point).toSet
    oppOrganSet = oppOrgan.map(organ => organ.point).toSet
    proteinMap = proteins.map(protein => (protein.point, protein)).toMap
    proteinSet = proteinMap.keySet
    oppTentacled = oppTentacled.filter(air)

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


    val myOrganCurrentHash = myOrgans.hashCode
    val oppOrganCurrentHash = oppOrgan.hashCode
    val wallsCurrentHash = oppOrgan.hashCode

    ryba = myOrganCurrentHash == myOrganHash && {
      ((myOrgan.length < oppOrgan.length) && (oppOrgan.length - myOrgan.length > 5)) ||
      (oppOrganCurrentHash == oppOrganHash && wallsCurrentHash == wallsHash)
    }

    for(_ <- 0 until requiredActionsCount) {
      val key = myOrganIterator.next()
      val organs = myOrgans(key)
      growCandidate = organs.maxBy(_.organId)

      adj(growCandidate.point)(graphFilter).foreach(graph.addEdge(growCandidate.point, _))

      val  (edgeTo, distTo) = graph.bfs(growCandidate.point)

      val command = if (ryba) getRybaCommand(organs) else getCommand(organs, requiredActionsCount, edgeTo, distTo)

      println(command)

    }

    myOrganHash = myOrganCurrentHash
    oppOrganHash = oppOrganCurrentHash
    wallsHash = wallsCurrentHash

  }
}
