package codingames.challenge

import math._
import scala.collection.immutable.SortedSet
import scala.collection.mutable
import scala.io.Source
import scala.util._
import scala.io.StdIn._

object Player extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
  val filename = "resources/grass/2.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines
  def readInt = if (data.hasNext) data.next.toInt else { System.exit(0); -1 }
  def readLine = if (data.hasNext) data.next else { System.exit(0); "" }
//----------------------------------------------------------------------------------------------------------------------
  type UnitMap = Map[Int, Robot]
  type BfsResult = (Array[Int], Array[Int])       // edges, distances
  type BfsMap = Map[Int, Map[Int, Array[Int]]]    // myUnitId -> Map[unitCoord -> (firstMove, pathLength)]
  type Reachable = Int => Boolean

  val Array(width, height) = (readLine split " ").filter(_ != "").map (_.toInt)

  def toNumber(x: Int, y: Int): Int = y * width + x % width
  def toNumber(point: (Int, Int)): Int = point._2 * width + point._1 % width
  def toMatrix(number: Int): (Int, Int) = (number % width, number / width)

  case class Tile(id: Int, scrapAmount: Int, owner: Int, units: Int, hasRecycler: Boolean, canBuild: Boolean, canSpawn: Boolean, inRangeOfRecycler: Boolean)
  case class Robot(id: Int, pos: Int)

  @inline
  def freeAdjacent(num: Int, tiles: Array[Tile], unitsIndices: Set[Int], available: Reachable) = {
    def correctly(xy: (Int, Int)) = {
      xy._1 >= 0 &&
        xy._1 < width &&
        xy._2 >= 0 &&
        xy._2 < height &&
        tiles(num).scrapAmount > 0 &&
        !unitsIndices.contains(num) &&
        available(num)
    }
    val (x, y) = toMatrix(num)
    List((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y))
      .withFilter(correctly)
      .map(toNumber)
  }


  def bfs(from: Robot, tiles: Array[Tile], unitIndices: Set[Int], available: Reachable): BfsResult = {
    val dimension = width * height
    val marked: Array[Boolean] = new Array[Boolean](dimension)
    val edgeTo = Array.fill[Int](dimension)(Int.MaxValue)
    val distTo = Array.fill[Int](dimension)(Int.MaxValue)
    val stack = mutable.Queue[Int]()

    stack.enqueue(from.pos)
    marked(from.pos) = true
    distTo(from.pos) = 0
    while (stack.nonEmpty) {
      val v = stack.dequeue
      freeAdjacent(v, tiles, unitIndices, available).filterNot(marked).foreach {
        w => {
          stack.enqueue(w)
          marked(w) = true
          edgeTo(w) = v
          distTo(w) = distTo(v) + 1
        }
      }
    }
    (edgeTo, distTo)
  }

  
  
  
  
  
  val alwaysTrue: Reachable = _ => true
  val size = width * height

  for (_ <- LazyList.from(0).takeWhile(_ < 201)) {
    val tiles = Array.ofDim[Tile](size)
    var myRobots = List.empty[Robot]
    var foeRobots = List.empty[Robot]
    var foeIndices = Set.empty[Int]     // maybe change to Map...
    var scrapList = SortedSet.empty[(Int, Int)]     // position, distance

    val Array(myMatter, oppMatter) = (readLine split "\\s+").map(_.toInt)
    Console.err.println(s"$myMatter $oppMatter")
    var myRobotIndex = 0
    var foeRobotIndex = 0

    for(i <- 0 until height; j <- 0 until width) {
      val position = i * width + j
      val Array(scrapAmount, owner, units, recycler, canBuild, canSpawn, inRangeOfRecycler) = (readLine split " ").withFilter(_ != "").map (_.toInt)
      Console.err.println(s"$scrapAmount $owner $units $recycler $canBuild $canSpawn $inRangeOfRecycler")
      tiles(position) = Tile(position, scrapAmount, owner, units, recycler == 1, canBuild == 1, canSpawn == 1, inRangeOfRecycler == 1)
      if (units > 0) {
        for (_ <- 0 until units) {
          if (owner == 1) {
            myRobots = Robot(myRobotIndex, position) :: myRobots
            myRobotIndex += 1
          } else if (owner == 0) {
            foeRobots = Robot(foeRobotIndex, position) :: foeRobots
            foeIndices += position
            foeRobotIndex += 1
          }
        }
      }
    }

//    val (hasRecycler, hasnotRecycler) = tiles.partition(tile => tile.hasRecycler)
//    val (canSpawn, cannotSpawn) = tiles.partition(tile => tile.canSpawn)
//    val (canBuild, cannotBuild) = tiles.partition(tile => tile.canBuild)
//    val (withScrap, withoutScrap) = tiles.partition(tile => tile.scrapAmount > 0 && !tile.hasRecycler)

    val freeScraps = tiles.withFilter(tile => tile.scrapAmount > 0 && !tile.hasRecycler).map(tile => (tile.id, tile.scrapAmount)).toMap
    val bfsMap = myRobots.map(robot => (robot.id, bfs(robot, tiles, foeIndices, alwaysTrue))).toMap

    println("WAIT")
  }
}