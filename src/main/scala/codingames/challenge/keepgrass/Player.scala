package codingames.challenge.keepgrass

import scala.collection.immutable.SortedSet
import scala.collection.mutable
import scala.io.Source
import scala.io.StdIn._

object Player extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
  val filename = "resources/grass/2.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines
  def readInt = if (data.hasNext) data.next.toInt else {System.exit(0); -1}
  def readLine = if (data.hasNext) data.next else {System.exit(0); ""}
//----------------------------------------------------------------------------------------------------------------------

  type UnitMap = Map[Int, Robot]
  type BfsResult = (Array[Int], Array[Int]) // edges, distances
  type UnitBfs = Map[Int, BfsResult] // edges, distances
  type BfsDist = Map[Int, Map[Int, Int]] // myRobotId -> Map[tileCoord -> distance]]
  type BfsMap = Map[Int, Map[Int, Array[Int]]] // myRobotId -> Map[tileCoord -> path]
  type BfsData = Map[Int, Map[Int, (Int, Int)]] // myRobotId -> Map[tileCoord -> (firstMove, pathLength)]
  type Reachable = Int => Boolean

  val Array(width, height) = (readLine split " ").filter(_ != "").map(_.toInt)
  Console.err.println(s"$width $height")

  def toNumber(x: Int, y: Int): Int = y * width + x % width
  def toNumber(point: (Int, Int)): Int = point._2 * width + point._1 % width
  def toMatrix(number: Int): (Int, Int) = (number % width, number / width)
  def toMatrixStr(number: Int): String = s"${number % width} ${number / width}"

  case class Tile(id: Int, scrapAmount: Int, owner: Int, units: Int, hasRecycler: Boolean, canBuild: Boolean, canSpawn: Boolean, inRangeOfRecycler: Boolean) {
    val (x, y) = toMatrix(id)
    def neighbours(tiles: Array[Tile]) = List((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)).filter(xy => xy._1 >= 0 &&
      xy._1 < width &&
      xy._2 >=0 &&
      xy._2 < height &&
      tiles(toNumber(xy)).scrapAmount > 0).map(toNumber)
    def neighboursNum(tiles: Array[Tile]) = neighbours(tiles)
    def freeSquares(unitMap: Set[Int], tiles: Array[Tile]) = this.neighbours(tiles).toSet -- unitMap
    def freeSquaresAsNum(units: Set[Int], tiles: Array[Tile]) = freeSquares(units, tiles)
  }
  case class Robot(id: Int, pos: Int)

  @inline
  def freeAdjacent(num: Int, tiles: Array[Tile], available: Reachable) = {
    def correctly(xy: (Int, Int)) = {
      xy._1 >= 0 &&
        xy._1 < width &&
        xy._2 >= 0 &&
        xy._2 < height &&
        tiles(num).scrapAmount > 0 &&
//        !unitsIndices.contains(toNumber(xy)) &&
        available(toNumber(xy))
    }

    val (x, y) = toMatrix(num)
    val res = List((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y))
      .withFilter(correctly)
      .map(toNumber)
    res
  }

  def closestTile(pos: Int, tiles: Array[Tile], available: Reachable, found: Reachable): Int = {
    val dimension = width * height
    val marked: Array[Boolean] = new Array[Boolean](dimension)
    val distTo = Array.fill[Int](dimension)(Int.MaxValue)
    val stack = mutable.Queue[Int]()

    stack.enqueue(pos)
    marked(pos) = true
    var dist = Int.MaxValue

    distTo(pos) = 0
    while (stack.nonEmpty) {
      val v = stack.dequeue
      if (found(v)) {
        stack.clear
        dist = distTo(v)
      } else {
        freeAdjacent(v, tiles, available).filterNot(marked).foreach {
          w => {
            stack.enqueue(w)
            marked(w) = true
            distTo(w) = distTo(v) + 1
          }
        }
      }
    }
    dist
  }

  def bfs(pos: Int, tiles: Array[Tile], /*unitIndices: Set[Int], */available: Reachable): BfsResult = {
    val dimension = width * height
    val marked: Array[Boolean] = new Array[Boolean](dimension)
    val edgeTo = Array.fill[Int](dimension)(Int.MaxValue)
    val distTo = Array.fill[Int](dimension)(Int.MaxValue)
    val stack = mutable.Queue[Int]()

    stack.enqueue(pos)
    marked(pos) = true
    distTo(pos) = 0
    while (stack.nonEmpty) {
      val v = stack.dequeue
      freeAdjacent(v, tiles, /*unitIndices, */available).filterNot(marked).foreach {
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



  def calcDistancesMap(myRobots: List[Robot], tiles: Array[Tile], bfsResMap: Map[Int, (Array[Int], Array[Int])]) = ???

  def calcBfsMap(mine: List[Robot], tiles: Array[Tile], bfsResMap: Map[Int, BfsResult]) = {
    def beginJourney(robotId: Int, to: Int) = {
      val (edges, distances) = bfsResMap(robotId)
      distances(to)
    }
    def toTile(robot: Robot) = {
      tiles.map(tile => (tile.id, beginJourney(robot.id, tile.id))).toMap
    }

    mine.map(me => {
//      val bfsRes = bfsResMap(me.id)
      (me.id, toTile(me))
    }).toMap
  }

  def calcBfsMapOld(mine: List[Robot], tilesWithScraps: List[Tile],
                 tiles: Array[Tile], bfsResMap: Map[Int, BfsResult],
                 unitMap: Set[Int], available: Reachable): BfsMap = {
    def bfsToUnit(bfs: BfsResult, tile: Tile): Option[Array[Int]] = {
      Some { tile.freeSquaresAsNum(unitMap, tiles)
        .withFilter(bfs._2(_) < Int.MaxValue)
        .map(neighbour => (neighbour, bfs._2(neighbour)))
      }.withFilter(_.nonEmpty)
       .map { targets =>
          val target = targets.minBy(_._2)
          var i = target._1
          var index = target._2
          val outArr = Array.ofDim[Int](target._2 + 1)
          outArr(index) = tile.id

          while(bfs._1(i) < Int.MaxValue) {
            index -= 1
            outArr(index) = i
            i = bfs._1(i)
          }

         outArr
       }
    }
    def bfsToUnits(myUnit: Robot, others: List[Tile], available: Reachable): Map[Int, Array[Int]] = {
      others.map(other => (other.id, bfsToUnit(bfsResMap(myUnit.id), other)))
        .collect {
          case unitPair if unitPair._2.isDefined => (unitPair._1, unitPair._2.get)
        }.toMap
    }

    mine.map(me => (me.id, bfsToUnits(me, tilesWithScraps, available))).toMap
  }

  def findTarget(robot: Robot, dataMap: BfsDist, huntMode: Boolean, found: Set[Int], tiles: Array[Tile]) = {
    if (huntMode) {
      val (command, target) = findClosestFoe(robot, dataMap, found, tiles)
      if (target == -1) findClosestScrap(robot, dataMap, found, tiles)
      else (command, target)
    } else {
      findClosestScrap(robot, dataMap, found, tiles)
    }
  }
  def findClosestScrap(robot: Robot, dataMap: BfsDist, found: Set[Int], tiles: Array[Tile]) = {
    findClosestOld(robot, dataMap, noMyScrapRepetitive(tiles, found))
  }
  def findClosestFoe(robot: Robot, dataMap: BfsDist, found: Set[Int], tiles: Array[Tile]) = {
    findClosestOld(robot, dataMap, foeOrNoMyScrapRepetitive(tiles, found))
  }

  def findClosestOld(robot: Robot, dataMap: BfsDist, f: ((Int, Int)) => Boolean) = {
    val closestTarget = dataMap(robot.id)
      .filterNot(f)
      .minByOption(_._2)
      .map(posDist => (s"MOVE 1 ${toMatrixStr(robot.pos)} ${toMatrixStr(posDist._1)} ", posDist._1))
      .getOrElse(("", -1))
    closestTarget
  }

  def recyclerProfit(pos: Int, tiles: Array[Tile]) = {
    val f = (pos: Int) => !withGrass(tiles)(pos) && !withRecyclers(tiles)(pos)
    val adjacents = freeAdjacent(pos, tiles, f)
    adjacents.map(adj => tiles(adj).scrapAmount).sum + tiles(pos).scrapAmount
  }

  def shouldSpawnRobot(myMatter: Int, tiles: Array[Tile], huntMode: Boolean) = {
    val robotNumber = myMatter / 10
    if (robotNumber > 0) {
      tiles.withFilter(_.canSpawn)
           .map(tile => (tile.id, closestTile(tile.id, tiles, available(tiles, huntMode), notMine(tiles, huntMode))))
           .minByOption(_._2)
           .map(target => s"SPAWN $robotNumber ${toMatrixStr(target._1)}")
           .getOrElse("")
//      tiles.find(_.canSpawn).map(tile => s"SPAWN $robotNumber ${toMatrixStr(tile.id)}").getOrElse("")
    } else ""
  }


  def shouldBuildRecycler(huntMode: Boolean, myMatter: Int, myRecyclersAmount: Int, tiles: Array[Tile]) = {
    if (huntMode) ("", myMatter) else
    if (myMatter >= 10 && myRecyclersAmount < 2) {
      val recyclers = tiles.withFilter(_.canBuild).map(tile => (tile, recyclerProfit(tile.id, tiles)))
      recyclers.maxByOption(_._2)
               .map(recycler => (s"BUILD ${toMatrixStr(recycler._1.id)}", myMatter - 10))
               .getOrElse(("", myMatter))
    } else ("", myMatter)
  }

  def findClosest(huntMode: Boolean, myRobots: List[Robot], tiles: Array[Tile], bfsMap: BfsDist) = {
    if (huntMode) {
      findClosestPairs(myRobots, tiles, bfsMap, tile => withGrassOrRecyclers(tiles)(tile.id) || mineOrNeutral(tiles)((tile.id, -1)))
    } else {
      findClosestPairs(myRobots, tiles, bfsMap, tile => withGrassOrRecyclers(tiles)(tile.id) || mine(tiles)((tile.id, -1)))
    }
  }

  def findClosestPairs(myRobots: List[Robot], tiles: Array[Tile], bfsMap: BfsDist, exclude: Tile => Boolean) = {
    var foundTarget = Set.empty[Int]
    var busyRobot = Set.empty[Int]
    var pairs = List.empty[(Robot, Tile)]

//    val withScraps = tiles.filterNot(tile => withGrassOrRecyclers(tiles)(tile.id) || mine(tiles)((tile.id, -1)))
    val targetTiles = tiles.filterNot(exclude)
    var robotScrapPairs = (for (robot <- myRobots; tile <- targetTiles) yield {
      (robot, tile)
    }).sortBy(rt => {
      bfsMap(rt._1.id)(rt._2.id)
    })

    while (robotScrapPairs.nonEmpty && busyRobot.size < myRobots.length) {
      robotScrapPairs = robotScrapPairs.dropWhile(rt => busyRobot.contains(rt._1.id)/* || foundTarget.contains(rt._2.id)*/)
      robotScrapPairs.headOption match {
        case Some(nextPair) =>
          foundTarget += nextPair._2.id
          busyRobot += nextPair._1.id
          pairs = nextPair :: pairs
          robotScrapPairs = robotScrapPairs.tail
        case None =>
      }
    }
    pairs.map(rt => (s"MOVE 1 ${toMatrixStr(rt._1.pos)} ${toMatrixStr(rt._2.id)}")).mkString(";")

//    val moves = (for (robot <- myRobots) yield {
//      val (str, target) = findTarget(robot, bfsMap, huntMode, foundTarget, tiles)
//      foundTarget += target
//      str.trim
//    }).mkString(";")
//    moves
  }

  def alwaysTrue(pos: Int) = true
  def notMine(tiles: Array[Tile], huntMode: Boolean)(pos: Int) = if (huntMode) withFoeUnits(tiles)(pos) else tiles(pos).owner == 0
  def withFoeUnits(tiles: Array[Tile])(pos: Int) = tiles(pos).units > 0 && tiles(pos).owner == 0
  def withGrass(tiles: Array[Tile])(pos: Int) = tiles(pos).scrapAmount <= 0
  def withRecyclers(tiles: Array[Tile])(pos: Int) = tiles(pos).hasRecycler
  def withGrassOrRecyclers(tiles: Array[Tile])(pos: Int) = withGrass(tiles)(pos) || withRecyclers(tiles)(pos)
  def available(tiles: Array[Tile], huntMode: Boolean)(pos: Int) = !withGrass(tiles)(pos) && !withRecyclers(tiles)(pos) && (huntMode || !withFoeUnits(tiles)(pos))
  def notTheSame(robot: Robot)(posDist: (Int, Int)) = posDist._1 == robot.pos
  def mine(tiles: Array[Tile])(posDist: (Int, Int)) = tiles(posDist._1).owner == 1
  def mineOrNeutral(tiles: Array[Tile])(posDist: (Int, Int)) = tiles(posDist._1).units <= 0 || tiles(posDist._1).owner == 1
  def noRepetitive(found: Set[Int])(posDist: (Int, Int)) = found.contains(posDist._1)
  def noMyScrapRepetitive(tiles: Array[Tile], found: Set[Int])(posDist: (Int, Int)) = noRepetitive(found)(posDist) || mine(tiles)(posDist)
  def foeOrNoMyScrapRepetitive(tiles: Array[Tile], found: Set[Int])(posDist: (Int, Int)) = mineOrNeutral(tiles)(posDist) || noRepetitive(found)(posDist)








  val size = width * height

  for (_ <- LazyList.from(0).takeWhile(_ < 201)) {
    val tiles = Array.ofDim[Tile](size)
    var myRobots = List.empty[Robot]
    var foeRobots = List.empty[Robot]
    var foeIndices = Set.empty[Int] // maybe change to Map...
    var scrapList = SortedSet.empty[(Int, Int)] // position, distance

    val Array(myMatter, oppMatter) = (readLine split "\\s+").map(_.toInt)
    Console.err.println(s"$myMatter $oppMatter")
    var myRobotIndex = 0
    var foeRobotIndex = 0
    var myRecyclersAmount = 0
    var myTiles = 0
    var foeTiles = 0

    for (i <- 0 until height; j <- 0 until width) {
      val position = i * width + j
      val Array(scrapAmount, owner, units, recycler, canBuild, canSpawn, inRangeOfRecycler) = (readLine split " ").withFilter(_ != "").map(_.toInt)
//      Console.err.println(s"$scrapAmount $owner $units $recycler $canBuild $canSpawn $inRangeOfRecycler")
      tiles(position) = Tile(position, scrapAmount, owner, units, recycler == 1, canBuild == 1, canSpawn == 1, inRangeOfRecycler == 1)
      myRecyclersAmount = if (recycler == 1 && owner == 1) myRecyclersAmount + 1 else myRecyclersAmount

      if (owner == 0) foeTiles += 1 else if (owner == 1) myTiles += 1

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

    val huntMode = (myRobots.length > foeRobots.length) || (myRobots.length == foeRobots.length && myTiles > foeTiles)
    Console.err.println(s"hunt=$huntMode")
    //    val (hasRecycler, hasnotRecycler) = tiles.partition(tile => tile.hasRecycler)
    //    val (canSpawn, cannotSpawn) = tiles.partition(tile => tile.canSpawn)
    //    val (canBuild, cannotBuild) = tiles.partition(tile => tile.canBuild)
    //    val (withScrap, withoutScrap) = tiles.partition(tile => tile.scrapAmount > 0 && !tile.hasRecycler)


//    val withoutRecyclers = withScraps.filter(!_.hasRecycler).toList
//    val withoutRecyclersData = withoutRecyclers.map(tile => (tile.id, tile.scrapAmount)).toMap
//    val withScrapsData = withScraps.map(tile => (tile.id, tile.scrapAmount)).toMap

    val bfsResMap = myRobots.map(robot => (robot.id, bfs(robot.pos, tiles, available(tiles, huntMode)))).toMap
    val bfsMap = calcBfsMap(myRobots, tiles, bfsResMap)

    val (buildRecyclers, newMatter) = shouldBuildRecycler(huntMode, myMatter, myRecyclersAmount, tiles)
    val buildRobot = shouldSpawnRobot(newMatter, tiles, huntMode)
    val moves = findClosest(huntMode, myRobots, tiles, bfsMap)
//    val bfsMap = calcBfsMap(myRobots, withScraps, tiles, bfsResMap, foeIndices, alwaysTrue)
//    val bfsMap = calcDistancesMap(myRobots, tiles, bfsResMap)
//    val bfsData = bfsMap.view.mapValues(value => )

    val targetStr = List(buildRecyclers, buildRobot, moves).filter(_.nonEmpty).mkString(";")
    println(targetStr)
  }
}
