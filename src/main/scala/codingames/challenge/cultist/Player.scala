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
  type BfsMap = Map[Int, Map[Int, Array[Int]]]    // myUnitId -> Map[unitCoord -> (firstMove, pathLength)]
  type Reachable = Int => Boolean

  case class Person(unitId: Int, isLeader: Boolean, hp: Int, x: Int, y: Int, owner: Int) {
    val asNumber = y * width + x % width
    val neighbours = List((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)).filter(xy => xy._1 >= 0 &&
      xy._1 < width &&
      xy._2 >=0 &&
      xy._2 < height &&
      matrix(toNumber(xy)) != STONE)
    val neighboursNum = neighbours.map(toNumber)

    def freeSquares(unitMap: UnitMap) = this.neighbours.toSet -- unitMap.keySet
    def freeSquaresAsNum(units: UnitMap) = freeSquares(units).map(toNumber)
    def distanceTo(unit: Person) = Math.abs(x - unit.x) + Math.abs(y - unit.y)
    def distanceTo(target: Int) = {
      val (unitX, unitY) = toMatrix(target)
      Math.abs(x - unitX) + Math.abs(y - unitY)
    }
    def distanceBfs(target: Person, bfsMap: BfsMap) = if (bfsMap.contains(unitId)) {
      if (bfsMap(unitId).contains(target.unitId)) Some(bfsMap(unitId)(target.unitId).length) else None
    } else None
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
  // Console.err.println(s"$myId")
  val Array(width, height) = (readLine split " ").filter(_ != "").map (_.toInt)
  // Console.err.println(s"$width $height")

  val lines = (for(_ <- 0 until height) yield readLine).toArray
  //  lines.foreach(Console.err.println)
  val matrix = lines.flatMap(_.map(symMap(_)).toArray)

  var bfsRes = mutable.Map.empty[Int, BfsResult]
  var units = List.empty[Person]

  def toNumber(x: Int, y: Int): Int = y * width + x % width
  def toNumber(point: (Int, Int)): Int = point._2 * width + point._1 % width
  def toMatrix(number: Int): (Int, Int) = (number % width, number / width)

  def freeAdjacent(num: Int, allUnitsMap: UnitMap, available: Reachable) = {
    val (x, y) = toMatrix(num)
    List((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)).withFilter(xy => xy._1 >= 0 &&
      xy._1 < width &&
      xy._2 >=0 &&
      xy._2 < height &&
      matrix(num) != STONE &&
      !allUnitsMap.contains((xy._1, xy._2)) &&
      available(num)
    ).map(toNumber)
  }


  def bfs(from: Person, unitMap: UnitMap, available: Reachable) = {
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
      freeAdjacent(v, unitMap, available).filterNot(marked).foreach {
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

  def bfsMap(mine: List[Person], enemy: List[Person], unitMap: UnitMap, available: Reachable): BfsMap = {
    def bfsToUnit(bfs: BfsResult, enemy: Person): Option[Array[Int]] = {
      Some { enemy.freeSquaresAsNum(unitMap)
        .withFilter(bfs._2(_) < Int.MaxValue)
        .map(neighbour => (neighbour, bfs._2(neighbour)))
      }.withFilter(_.nonEmpty)
        .map { targets =>
          val target = targets.minBy(_._2)
          var i = target._1
          var index = target._2
          val outArr = Array.ofDim[Int](target._2)

          while(bfs._1(i) < Int.MaxValue) {
            index -= 1
            outArr(index) = i
            i = bfs._1(i)
          }
          outArr
        }
    }
    def bfsToUnits(myUnit: Person, others: List[Person], available: Reachable): Map[Int, Array[Int]] = {
      val unitBfs = bfs(myUnit, unitMap, available)
      bfsRes.put(myUnit.unitId, unitBfs)

      others.map(other => (other.unitId, bfsToUnit(unitBfs, other)))
        .collect {
          case unitPair if unitPair._2.isDefined => (unitPair._1, unitPair._2.get)
        }.toMap
    }

    mine.map(me => (me.unitId, bfsToUnits(me, enemy, available))).toMap
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
        if (matrix(toNumber(currentX, currentY)) == STONE
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
        if (matrix(toNumber(currentX, currentY)) == STONE
          || unitMap.contains((currentX, currentY))
          && unitMap((currentX, currentY)).hp > 0) {
          out = (currentX, currentY)
          exit = true
        }
      }
    }
    out
  }

  def isDanger(enemy: List[Person], target: Int, unitMap: UnitMap) = enemy.exists(unit => unit.canShoot(target, unitMap) && unit.distanceTo(target) < 7)

  @inline
  def isSafe(target: Int, safeSquares: Set[Int]) = safeSquares.contains(target)

  def closestSafe(unit: Person, safeSquares: Set[Int]) = {
    bfsRes(unit.unitId)._2.zipWithIndex
      .filter(square => isSafe(square._2, safeSquares))
      .minByOption(_._1)
      .map(_._2)
  }


  def convert(myLeader: Option[Person], neutral: List[Person]) = {
    myLeader match {
      case Some(leader) =>
        leader.neighbourUnits(neutral).headOption.map(target => s"${leader.unitId} CONVERT ${target.unitId}")
      case _ => None
    }
  }
  def move(mine: List[Person], enemy: List[Person], bfsMap: BfsMap, safeSquares: Set[Int]) = {
    val underAttack = mine.filterNot(warrior => isSafe(warrior.asNumber, safeSquares))
    val mostDamaged = underAttack.minByOption(_.hp)

    mostDamaged.flatMap(warrior => {
      closestSafe(warrior, safeSquares).map(safeSquare => {
        val (x,y) = toMatrix(safeSquare)
        s"${warrior.unitId} MOVE $x $y"
      })
    }).orElse {
      val cartesian = for (my <- mine; their <- enemy) yield {
        val distance = my.distanceBfs(their, bfsMap)
        val stronger = my.hp >= their.hp
        (my, their, distance, stronger)
      }
      val closestWeaker = cartesian.filter(pair => pair._3.isDefined && pair._4)
      if (closestWeaker.isEmpty) None else {
        val closest = closestWeaker.minBy(_._3)
        Some(s"${closest._1.unitId} MOVE ${closest._2.x} ${closest._2.y}")
      }
    }
  }

  def moveLeader(myLeader: Option[Person], neutral: List[Person], enemy: List[Person], bfs: BfsMap, unitMap: UnitMap, safeSquares: Set[Int]) = {
    myLeader match {
      case Some(leader) =>
        val leadBfs = bfs(leader.unitId)
        val isSafeNow = isSafe(leader.asNumber, safeSquares)
        val safeForMove = leader.freeSquaresAsNum(unitMap).filter(square => isSafe(square, safeSquares))
        val safeNeutral = neutral.filter(unit => leadBfs.contains(unit.unitId) && safeForMove.contains(leadBfs(unit.unitId).head))
        val closestAnyNeutral = neutral.minByOption(unit => if (leadBfs.contains(unit.unitId)) leadBfs(unit.unitId).length else Int.MaxValue)

        if (closestAnyNeutral.isDefined && leadBfs.contains(closestAnyNeutral.get.unitId) && leadBfs(closestAnyNeutral.get.unitId).length < 3) Some {
          val (x,y) = toMatrix(leadBfs(closestAnyNeutral.get.unitId).head)
          s"${leader.unitId} MOVE $x $y"
        } else if (safeNeutral.isEmpty && isSafeNow) None
        else if (safeNeutral.nonEmpty) {
          val closestNeutral = safeNeutral.minBy(neutral => leadBfs(neutral.unitId).length)
          if (leadBfs(closestNeutral.unitId).length > 7) None
          else Some {
            val (x, y) = toMatrix(leadBfs(closestNeutral.unitId).head)
            s"${leader.unitId} MOVE $x $y"
          }
        } else {
          closestSafe(leader, safeSquares).map(square => {
            val (x,y) = toMatrix(square)
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
    val closestReachable = cartesian.filter(data => data._4 && data._3 < 7)
    if (closestReachable.isEmpty) None else {
      val (me, enemy, _, _) = closestReachable.minBy(data => data._2.hp / (7 - data._3))
      Some(s"${me.unitId} SHOOT ${enemy.unitId}")
    }
  }

  // game loop
  while(true) {
    val numOfUnits = readLine.toInt // The total number of units on the board
    // Console.err.println(s"$numOfUnits")
    bfsRes.clear

    units = (for(_ <- 0 until numOfUnits) yield {
      // unitId: The unit's ID
      // unitType: The unit's type: 0 = Cultist, 1 = Cult Leader
      // hp: Health points of the unit
      // x: X coordinate of the unit
      // y: Y coordinate of the unit
      // owner: id of owner player
      val Array(unitId, unitType, hp, x, y, owner) = (readLine split " ").withFilter(_ != "").map (_.toInt)
      // Console.err.println(s"$unitId $unitType $hp $x $y $owner")
      Person(unitId, unitType == 1, hp, x, y, owner)
    }).toList

    val unitMap = units.map(unit => ((unit.x, unit.y), unit)).toMap

    val personByOwners = units.groupBy(_.owner)
    val mine = personByOwners(myId)
    val enemy = personByOwners(1 - myId)
    val (enemyLeaderList, enemyWithoutLeader) = enemy.partition(_.isLeader)
    val enemyLeader = enemyLeaderList.headOption
    val neutral = personByOwners.getOrElse(2, List.empty)
    val (myLeaderList, mineWithoutLeader) = mine.partition(_.isLeader)
    val myLeader = myLeaderList.headOption

    val (_, safeSquares) = matrix.indices.toSet.partition(square => isDanger(enemyWithoutLeader, square, unitMap))


    val allBfs = bfsMap(mine, enemy ++ neutral, unitMap, _ => true)
    //    val safeBfs = bfsMap(myLeaderList, neutral, unitMap, safeSquares.contains)

    //    Console.err.println(s"danger: ${dangerSquares.map(toMatrix).mkString(", ")}")
    //    Console.err.println(s"safe: ${safeSquares.map(toMatrix).mkString(", ")}")

    val safeLeader = myLeader.exists(leader => isSafe(leader.asNumber, safeSquares))

    val action = convert(myLeader, neutral)
      .orElse(if (safeLeader) shoot(mineWithoutLeader, enemy, unitMap) else moveLeader(myLeader, neutral, enemy, allBfs, unitMap, safeSquares))
      .orElse(if (safeLeader) moveLeader(myLeader, neutral, enemy, allBfs, unitMap, safeSquares) else shoot(mineWithoutLeader, enemy, unitMap))
      .orElse(move(mineWithoutLeader, enemy, allBfs, safeSquares))
      .getOrElse("WAIT")

    // WAIT | unitId MOVE x y | unitId SHOOT target| unitId CONVERT target
    println(action)
  }
}