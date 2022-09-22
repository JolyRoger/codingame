package codingames.challenge.cultist

import math._
import scala.io.Source
import scala.util._
import scala.io.StdIn._
import scala.collection.mutable

/**
 * Convert neutral units and attack enemy ones
 **/
object Player extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
  val filename = "resources/cultist/1.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines
  def readInt = if (data.hasNext) data.next.toInt else { System.exit(0); -1 }
  def readLine = if (data.hasNext) data.next else { System.exit(0); "" }
//----------------------------------------------------------------------------------------------------------------------
  type UnitMap = Map[(Int, Int), Person]
  type BfsResult = (Array[Int], Array[Int])
  type BfsMap = Map[Int, Map[Int, (Int, Int)]]    // myUnitId -> Map[unitCoord -> (firstMove, pathLength)]

  case class Person(unitId: Int, isLeader: Boolean, hp: Int, x: Int, y: Int, owner: Int) {
    val asNumber = y * width + x % width
    val neighbours = List((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)).filter(xy => xy._1 >= 0 &&
      xy._1 < width &&
      xy._2 >=0 &&
      xy._2 < height &&
      matrix(xy._2)(xy._1) != STONE)
    val neighboursNum = neighbours.map(toNumber)

    def freeSquares(unitMap: UnitMap) = this.neighbours.toSet -- unitMap.keySet
    def freeSquaresAsNum(units: UnitMap) = freeSquares(units).map(toNumber)
    def distanceTo(unit: Person) = Math.abs(x - unit.x) + Math.abs(y - unit.y);
    def neighbourUnits(units: List[Person]) = {
      val personMap = units.map(person => ((person.x, person.y), person)).toMap
      personMap.keySet.intersect(this.neighbours.toSet).map(personMap(_))
    }
    def canShoot(target: Int, unitMap: UnitMap) = {
      if (isLeader || owner == 2) false else {
        val (targetX, targetY) = toMatrix(target)
        val shootResult = checkBulletPath(x, y, targetX, targetY, unitMap)
        shootResult._1 == targetX && shootResult._2 == targetY
      }
    }
  }

  val AIR = 0
  val STONE = -2
  val symMap = Map('.' -> AIR, 'x' -> STONE)
  val myId = readLine.toInt // 0 - you are the first player, 1 - you are the second player
  Console.err.println(s"$myId")
  // width: Width of the board
  // height: Height of the board
  val Array(width, height) = (readLine split " ").filter(_ != "").map (_.toInt)
  Console.err.println(s"$width $height")
  val matrix = for(_ <- 0 until height) yield {
    val line = readLine
    Console.err.println(s"$line")
    line.map(symMap(_))
  }
  var bfsRes = mutable.Map.empty[Int, BfsResult]
  var units = List.empty[Person]

  def toNumber(point: (Int, Int)): Int = point._2 * width + point._1 % width
  def toMatrix(number: Int): (Int, Int) = (number % width, number / width)

  def freeAdjacent(num: Int, allUnitsMap: UnitMap) = {
    val (x, y) = toMatrix(num)
    List((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)).withFilter(xy => xy._1 >= 0 &&
      xy._1 < width &&
      xy._2 >=0 &&
      xy._2 < height &&
      matrix(xy._2)(xy._1) != STONE &&
      !allUnitsMap.contains((xy._1, xy._2))
    ).map(toNumber)
  }


  def bfs(from: Person, unitMap: UnitMap) = {
    val dimension = width * height
    val marked: Array[Boolean] = new Array[Boolean](dimension)
    val edgeTo = Array.fill[Int](dimension)(Int.MaxValue)
    val distTo = Array.fill[Int](dimension)(Int.MaxValue)
    val q = mutable.Queue[Int]()

    q.enqueue(from.asNumber)
    marked(from.asNumber) = true
    distTo(from.asNumber) = 0
    while (q.nonEmpty) {
      val v = q.dequeue
      freeAdjacent(v, unitMap).filterNot(marked).foreach {
        w => {
          q.enqueue(w)
          marked(w) = true
          edgeTo(w) = v
          distTo(w) = distTo(v) + 1
        }
      }
    }
    (edgeTo, distTo)
  }

  def bfsMap(mine: List[Person], enemy: List[Person], unitMap: UnitMap): BfsMap = {
    def bfsToUnit(bfs: BfsResult, enemy: Person) = {
      Some { enemy.freeSquaresAsNum(unitMap)
        .withFilter(bfs._2(_) < Int.MaxValue)
        .map(neighbour => (neighbour, bfs._2(neighbour)))
      }.withFilter(_.nonEmpty)
        .map { targets =>
          val target = targets.minBy(_._2)
          var i = target._1
          var next = i
          var index = target._2

          while(bfs._1(i) < Int.MaxValue) {
            index -= 1
            next = i
            i = bfs._1(i)
          }
          (next, target._2)
        }
    }
    def bfsToUnits(myUnit: Person, others: List[Person]): Map[Int, (Int, Int)] = {
      val unitBfs = bfs(myUnit, unitMap)
      bfsRes.put(myUnit.unitId, unitBfs)

      others.map(other => (other.unitId, bfsToUnit(unitBfs, other)))
        .collect {
          case unitPair if unitPair._2.isDefined => (unitPair._1, unitPair._2.get)
        }.toMap
    }

    mine.map(me => (me.unitId, bfsToUnits(me, enemy))).toMap
  }

  def checkBulletPath(startX: Int, startY: Int, targetX: Int, targetY: Int, unitMap: Map[(Int, Int), Person]) =
    if (startY < targetY) bresenhamForward(startX, startY, targetX, targetY, unitMap: Map[(Int, Int), Person])
    else bresenhamBackward(startX, startY, targetX, targetY, unitMap: Map[(Int, Int), Person])

  private def bresenhamForward(startX: Int, startY: Int, targetX: Int, targetY: Int, unitMap: Map[(Int, Int), Person]): (Int, Int) = {
    var exit = false
    var out = (-1, -1)
    var x0 = 0
    var y0 = 0
    var x1 = 0
    var y1 = 0
    x0 = startX
    y0 = startY
    x1 = targetX
    y1 = targetY
    val dx = Math.abs(x1 - x0)
    val dy = Math.abs(y1 - y0)
    val sx = if (x0 < x1) 1 else -1
    val sy = if (y0 < y1) 1 else -1
    var err = dx - dy
    var e2 = 0
    var currentX = x0
    var currentY = y0
    while (!exit) {
      e2 = 2 * err
      if (e2 > -1 * dy) {
        err -= dy
        currentX += sx
      }
      if (e2 < dx) {
        err += dx
        currentY += sy
      }
      if (currentX == x1 && currentY == y1) {
        out = (targetX, targetY)
        exit = true
      } else {
        if (matrix(currentY)(currentX) == STONE
          || unitMap.contains((currentX, currentY))
          && unitMap((currentX, currentY)).hp > 0) {
          out = (currentX, currentY)
          exit = true
        }
      }
    }
    out
  }

  private def bresenhamBackward(startX: Int, startY: Int, targetX: Int, targetY: Int, unitMap: Map[(Int, Int), Person]): (Int, Int) = {
    var exit = false
    var out = (-1, -1)
    var x0 = 0
    var y0 = 0
    var x1 = 0
    var y1 = 0
    x0 = targetX
    y0 = targetY
    x1 = startX
    y1 = startY
    val dx = Math.abs(x1 - x0)
    val dy = Math.abs(y1 - y0)
    val sx = if (x0 < x1) 1 else -1
    val sy = if (y0 < y1) 1 else -1
    var err = dx - dy
    var e2 = 0
    var currentX = x0
    var currentY = y0
    while (!exit) {
      e2 = 2 * err
      if (e2 > -1 * dy) {
        err -= dy
        currentX += sx
      }
      if (e2 < dx) {
        err += dx
        currentY += sy
      }
      if (currentX == x1 && currentY == y1) {
        out = (targetX, targetY)
        exit = true
      } else {
        if (matrix(currentY)(currentX) == STONE
          || unitMap.contains((currentX, currentY))
          && unitMap((currentX, currentY)).hp > 0) {
          out = (currentX, currentY)
          exit = true
        }
      }
    }
    out
  }






  def convert(myLeader: Option[Person], neutral: List[Person]) = {
    myLeader match {
      case Some(leader) =>
        leader.neighbourUnits(neutral).headOption.map(target => s"${leader.unitId} CONVERT ${target.unitId}")
      case _ => None
    }
  }
  def move(mine: List[Person], enemy: List[Person]) = {
    val cartesian = for (my <- mine; their <- enemy) yield {
      val distance = my.distanceTo(their)
      val stronger = my.hp >= their.hp
      (my, their, distance, stronger)
    }
    val closestWeaker = cartesian.filter(_._4)
    if (closestWeaker.isEmpty) None else {
      val closest = closestWeaker.minBy(_._3)
      Some(s"${closest._1.unitId} MOVE ${closest._2.x} ${closest._2.y}")
    }
  }
  def moveLeader(myLeader: Option[Person], neutral: List[Person], enemy: List[Person], bfs: BfsMap, unitMap: UnitMap) = {
    def isDanger(target: Int) = enemy.exists(unit => unit.canShoot(target, unitMap))
    myLeader match {
      case Some(leader) =>
        val leadBfs = bfs(0)
        val isSafeNow = !isDanger(leader.asNumber)
        val safeForMove = leader.freeSquaresAsNum(unitMap).filterNot(square => isDanger(square))
        val safeNeutral = neutral.filter(unit => leadBfs.contains(unit.unitId) && safeForMove.contains(leadBfs(unit.unitId)._1))

        if (safeNeutral.isEmpty && isSafeNow) None
        else if (safeNeutral.nonEmpty) {
          val closestNeutral = safeNeutral.minBy(neutral => leadBfs(neutral.unitId)._2)
          if (leadBfs(closestNeutral.unitId)._2 > 7) None
          else Some {
            val (x, y) = toMatrix(leadBfs(closestNeutral.unitId)._1)
            s"${leader.unitId} MOVE $x $y"
          }
        } else {
          bfsRes(leader.unitId)._2.zipWithIndex
            .filter(square => !isDanger(square._2))
            .minByOption(_._1)
            .map(square => {
              val (x,y) = toMatrix(square._2)
              s"${leader.unitId} MOVE $x $y"
            })
        }
      case _ => None
    }
  }

  def shoot(mine: List[Person], enemy: List[Person], unitMap: Map[(Int, Int), Person]) = {
    val cartesian = for (my <- mine; their <- enemy) yield {
      val distance = my.distanceTo(their)
      val reachable = my.canShoot(their.asNumber, unitMap)
      (my, their, distance, reachable)
    }
    val closestReachable = cartesian.filter(_._4)
    if (closestReachable.isEmpty) None else {
      val closest = closestReachable.minBy(_._3)
      if (closest._3 < 7) Some(s"${closest._1.unitId} SHOOT ${closest._2.unitId}")
      else None
    }
  }

  // game loop
  while(true) {
    val numOfUnits = readLine.toInt // The total number of units on the board
    Console.err.println(s"$numOfUnits")
    bfsRes.clear

    units = (for(_ <- 0 until numOfUnits) yield {
      // unitId: The unit's ID
      // unitType: The unit's type: 0 = Cultist, 1 = Cult Leader
      // hp: Health points of the unit
      // x: X coordinate of the unit
      // y: Y coordinate of the unit
      // owner: id of owner player
      val Array(unitId, unitType, hp, x, y, owner) = (readLine split " ").withFilter(_ != "").map (_.toInt)
      Console.err.println(s"$unitId $unitType $hp $x $y $owner")
      Person(unitId, unitType == 1, hp, x, y, owner)
    }).toList

    val unitMap = units.map(unit => ((unit.x, unit.y), unit)).toMap

    val personByOwners = units.groupBy(_.owner)
    val mine = personByOwners(myId)
    val enemy = personByOwners(1 - myId)
    val neutral = personByOwners.getOrElse(2, List.empty)
    val (myLeaderList, mineWithoutLeader) = mine.partition(_.isLeader)
    val myLeader = myLeaderList.headOption
    val enemyLeader = enemy.find(_.isLeader)

    val allBfs = bfsMap(mine, enemy ++ neutral, unitMap)
    //    Console.err.println(s"$allBfs")

    val action = convert(myLeader, neutral)
      .orElse(moveLeader(myLeader, neutral, enemy, allBfs, unitMap))
      .orElse(shoot(mineWithoutLeader, enemy, unitMap))
      .orElse(move(mine, enemy))
      .getOrElse("WAIT")

    // WAIT | unitId MOVE x y | unitId SHOOT target| unitId CONVERT target
    println(action)
  }
}