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
    val ROOT, BASIC = Value
  }

  import OrganType._
  import OrganDir._

  trait Tile {
    def x: Int
    def y: Int
  }
  case class Wall(x: Int, y: Int) extends Tile
  case class Organ(organId: Int, x: Int, y: Int, orgType: OrganType, orgDir: OrganDir, organParentId: Int, organRootId: Int) extends Tile {
    def grow(x: Int, y: Int) = s"GROW $organId $x $y BASIC"
  }
  case class Protein(x: Int, y: Int) extends Tile

  var walls = List.empty[Wall]
  var myOrgan = List.empty[Organ]
  var oppOrgan = List.empty[Organ]
  var proteins = List.empty[Protein]

  var growCandidate: Organ = _

  // width: columns in the game grid
  // height: rows in the game grid
  val Array(width, height) = (readLine split " ").withFilter(_ != "").map (_.toInt)
  Console.err.println(s"$width $height")




// ----------------------------- GRAPH ---------------------------------------------------------------------------------
  class Graph(N: Int) {
    val adj = (for (_ <- 0 until N) yield List[Int]()).toArray

    def n = N

    def neighbourCandidates(x: Int, y: Int) = {
      List((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)).filter {
        xy => xy._1 >= 0 &&
          xy._1 < width &&
          xy._2 >= 0 &&
          xy._2 < height
      }
    }

    def addEdge(v: Int, w: Int) {
      adj(v) = w :: adj(v)
      adj(w) = v :: adj(w)
    }

    def cutEdge(v: Int, w: Int): Unit = {
      adj(v) = adj(v).filter(_ != w)
      adj(w) = adj(w).filter(_ != v)
    }

    def bfs(s: Int) = {
      val marked: Array[Boolean] = new Array[Boolean](n)
      val edgeTo = Array.fill[Int](n)(Int.MaxValue)
      val distTo = Array.fill[Int](n)(Int.MaxValue)
      val q = mutable.Queue[Int]()
      var i = 0

      q.enqueue(s)
      marked(s) = true
      distTo(s) = 0
      while (q.nonEmpty) {
        val v = q.dequeue
        i = i + 1
        adj(v).filterNot(marked).foreach(
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

    def path(from: Int, to: Int, edgeTo: Array[Int]): List[Int] = if (from == to) List(to) else List(from) ::: path(edgeTo(from), to, edgeTo)

    def sortedPathAdj(path: List[Int]) =
      (for (i <- adj.indices if path.contains(i)) yield i -> adj(i)).sortWith(_._2.size < _._2.size).toMap

    def findNeighbour(node: Int, path: List[Int]): Int = {
      for (i <- path.indices) {
        if (path(i) == node) {
          return if (i > 0) path(i - 1) else path(i + 1)
        }
      }
      -1
    }
  }
// ----------------------------- GRAPH ---------------------------------------------------------------------------------



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

  def clean = {
    walls = List.empty
    myOrgan = List.empty
    oppOrgan = List.empty
    proteins = List.empty
  }

// ---------------------------------------------------------------------------------------------------------------------

  val g = new Graph(width * height)

// -------- game loop ------------------------------
  for (step <- LazyList.from(0).takeWhile(_ < 100)) {
    val entityCount = readLine.toInt
    if (debug) Console.err.println(s"$entityCount")
    Console.err.println(s"step=$step")
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

      _type match {
        case "WALL" =>
          val wall = Wall(x, y)
          walls = wall :: walls
        case "ROOT" =>
          val organ = Organ(organId, x, y, ROOT, organDir, organParentId, organRootId)
          if (owner == 1) myOrgan ::= organ else oppOrgan ::= organ
        case "BASIC" =>
          val organ = Organ(organId, x, y, BASIC, organDir, organParentId, organRootId)
          if (owner == 1) myOrgan ::= organ  else oppOrgan ::= organ
        case "A" =>
          val protein = Protein(x, y)
          proteins = protein :: proteins
      }
    }
    // myD: your protein stock
    val Array(myA, myB, myC, myD) = (readLine split " ").filter(_ != "").map (_.toInt)
    if (debug) Console.err.println(s"$myA $myB $myC $myD")

    // oppD: opponent's protein stock
    val Array(oppA, oppB, oppC, oppD) = (readLine split " ").filter(_ != "").map (_.toInt)
    if (debug) Console.err.println(s"$oppA $oppB $oppC $oppD")

    val requiredActionsCount = readLine.toInt // your number of organisms, output an action for each one in any order
    if (debug) Console.err.println(s"$requiredActionsCount")

    for(_ <- 0 until requiredActionsCount) {
      // Write an action using println
      // To debug: Console.err.println("Debug messages...")
      growCandidate = myOrgan.reverse.head
      val closestProtein = proteins.minByOption(protein => euclidean((protein.x, protein.y), (growCandidate.x, growCandidate.y)))
      println {
        s"${closestProtein
            .map(protein => s"GROW ${growCandidate.organId} ${protein.x} ${protein.y} BASIC")
            .getOrElse("WAIT")}"
      }

    }
  }
}
