package codingame.challenge.iceandfire

import java.io.File

import math._
import scala.io.Source
import scala.util._

/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/
object Player extends App {
//------------------------------------------VARIABLES-------------------------------------------------------------------
  val limit = 7
  var step = 0
  val isTest = true
  val isDebug = false
  val output = true
  val outputLikeInput = false
//------------------------------------------FILE ENTRY------------------------------------------------------------------
  val filename = "iceandfire/iceandfire0.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines

  def readInt = if (data.hasNext) data.next.toInt else -1
  def readLine = if (data.hasNext) data.next else "\0"


  //------------------------------------------CLASSES---------------------------------------------------------------------

  import scala.collection.mutable

  case class Board(boardMatrix: Array[Array[Char]]) {

    val w = 12
    val N = w * w

    def toMatrix(number: Int): (Int, Int) = (number % w, number / w)
    implicit def toNumber(point: (Int, Int)): Int = point._2 * w + point._1 % w

    def allClosest(x: Int, y: Int) = List((x-1, y), (x, y-1), (x+1, y), (x, y+1)).filter(
      square => square._1 >= 0 && square._2 >= 0 && square._1 < 12 && square._2 < 12 &&
        boardMatrix(square._2)(square._1) != '#').map(toNumber)

    val adj = new Array[List[Int]](w * w)

    for (row <- 0 until 12; col <- 0 until 12; if boardMatrix(row)(col) != '#')
      adj(toNumber((col, row))) = allClosest(col, row)

    val topLeftBfs = bfs(0)
    val bottomDownBfs = bfs(w * w - 1)

    def bfs(s: Int) = {
      val marked: Array[Boolean] = new Array[Boolean](N)
      val edgeTo = Array.fill[Int](N)(Int.MaxValue)
      val distTo = Array.fill[Int](N)(Int.MaxValue)
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
  }


  import scala.math.{pow, sqrt}

  case class World(numMineSpots: Int, mineSpotsData: List[Array[Int]]) {
    //  def turnTo(id: Int, str: String) =

    // World class
    var buildingcount = 0
    var unitcount = 0
    var board = Array.empty[String]
    var boardMatrix: Array[Array[Char]] = Array.empty
    var mySoldiers: List[Soldier] = List.empty
    var enemySoldiers: List[Soldier] = List.empty

    def closestEuclidean(p: (Int, Int), items: List[(Int, Int)]) = if (items.isEmpty) (p._1, p._2) else items.minBy(item => euclidean(p, item))
    def euclidean(a: (Int, Int), b: (Int, Int)) = sqrt(pow(b._1 - a._1, 2) + pow(b._2 - a._2, 2))

    //  hqGraph._2(board.toNumber((p._1, p._2)))
    def closestEnemySoldierTo2(x: Int, y: Int, dist: Array[Int], board: Board) = enemySoldiers.
      map(unit => (unit, dist(board.toNumber((unit.x, unit.y)))))
      .sortBy(rec => rec._2).headOption
    def closestEnemySoldierTo(x: Int, y: Int) = enemySoldiers.map(es => (es, euclidean((es.x, es.y), (x, y)))).headOption
    def closestWithoutUnits(x: Int, y: Int, soldiers: List[Soldier], sym: List[Char]) = closest(x, y, sym).filterNot(point => soldiers.exists(unit => unit.x == point._1 && unit.y == point._2))
    def closestWithUnits(x: Int, y: Int, units: List[Soldier], sym: List[Char]) =
      closest(x, y, sym).withFilter(
        point => units.exists(unit => unit.x == point._1 && unit.y == point._2)
      ).map(point => (point, units.find(unit => unit.x == point._1 && unit.y == point._2).get))

    def closest(x: Int, y: Int, sym: List[Char]) = List((x-1, y), (x, y-1), (x+1, y), (x, y+1)).filter(
      square => square._1 >= 0 && square._2 >= 0 && square._1 < 12 && square._2 < 12 &&
        sym.contains(boardMatrix(square._2)(square._1)))
    def closestEmpty(x: Int, y: Int) = closest(x, y, List('.'))
    def closestForConqueror(x: Int, y: Int) = closestWithoutUnits(x, y, enemySoldiers, List('.', 'X', 'x'))
    def closestFree(x: Int, y: Int) = closestWithoutUnits(x, y, mySoldiers, List('.', 'O'))
    def closestEnemy(x: Int, y: Int) = closest(x, y, List('X'))
    def closestEnemySoldiers(x: Int, y: Int) = closestWithUnits(x, y, enemySoldiers, List('X'))

    def containsMyUnit(x: Int, y: Int) = mySoldiers.exists(soldier => soldier.x == x && soldier.y == y)
    def doesNotContainMyUnit(x: Int, y: Int) = !mySoldiers.exists(soldier => soldier.x == x && soldier.y == y)
    def containsEnemyUnit(x: Int, y: Int) = enemySoldiers.exists(soldier => soldier.x == x && soldier.y == y)
    def doesNotContainEnemyUnit(x: Int, y: Int) = !enemySoldiers.exists(soldier => soldier.x == x && soldier.y == y)

    def adjTo(x: Int, y: Int, sym: Char) = closest(x, y, List(sym)).nonEmpty
    def myClosestTo(p: (Int, Int)) = closestEuclidean(p, allWith(List('.', 'O'), (x, y) => {
      doesNotContainMyUnit(x, y) && adjTo(x, y, 'O')
    }))
    def all(sym: List[Char]) = (for (i <- 0 until 12; j <- 0 until 12; if sym.contains(boardMatrix(i)(j))) yield (j, i)).toList
    def allWith(sym: List[Char], filter: (Int, Int) => Boolean) = (for (i <- 0 until 12; j <- 0 until 12; if sym.contains(boardMatrix(i)(j)) && filter(j, i)) yield (j, i)).toList
    def allFree = allWith(List('.', 'O'), doesNotContainMyUnit)
    def allConquerorTarget = allWith(List('.', 'X'), doesNotContainEnemyUnit)
    def freeToMove(x: Int, y: Int) = (for (i <- 0 until 12; j <- 0 until 12; if boardMatrix(j)(i) == '.' || (boardMatrix(j)(i) == 'X' && !enemySoldiers.exists(soldier => i == soldier.x && j == soldier.y) )) yield (j, i)).toList

    //  def calculateGold

    def printBoard = board.foreach(Console.err.println)
    def print = {
      Console.err.println(s"numMineSpots=$numMineSpots")
      mineSpotsData.foreach(ms => Console.err.println(s"\t${ms(0)} ${ms(1)}"))
      Console.err.println(s"buildingcount=$buildingcount")
      Console.err.println(s"unitcount=$unitcount")
      printBoard
    }
  }


  case class Soldier(id: Int, var level: Int, world: World, x: Int, y: Int) {
    def canKill(myLevel: Int, enemyLevel: Int) = myLevel > enemyLevel || (myLevel == 3 && enemyLevel == 3)
    def findCanKill(enemyList: List[((Int, Int), Soldier)]) = enemyList.find(enemySoldier => canKill(level, enemySoldier._2.level))
    def closestEmpty = world.closestEmpty(x, y)
    def move: Option[(Int, Int, Int)] = {
      val target = findCanKill(world.closestEnemySoldiers(x ,y))
      if (target.isDefined) {
        Some((id, target.get._1._1, target.get._1._2))
      } else None
    }

    override def toString = s"$level:[$x,$y]"
  }


  class Guardian(id: Int, level: Int, world: World, x: Int, y: Int, me: Me) extends Soldier(id, level, world, x, y) {
    override def move = Some((id, x, y))
    /*
      override def move = super.move match {
        case None =>
          val hqFree = world.closestFree(me.headquarters._1, me.headquarters._2)
          if (hqFree.nonEmpty) Some((id, hqFree.head._1, hqFree.head._2))
          else {
    //        me.turnTo(id, "Scout")
            Some((id, x, y))
          }
        case Some(m) => Some(m)
      }
    */
  }


  class Scout(id: Int, level: Int, world: World, x: Int, y: Int, me: Me) extends Soldier(id, level, world, x, y) {
    override def move = super.move match {
      case None => Some((id, me.enemyHeadquarters._1, me.enemyHeadquarters._2))
      case Some(m) => Some(m)
    }
  }


  class Conqueror(id: Int, level: Int, world: World, x: Int, y: Int, me: Me) extends Soldier(id, level, world, x, y) {
    override def move = super.move match {
      case None =>
        val goal = world.closestForConqueror(x, y).headOption.getOrElse((me.enemyHeadquarters._1, me.enemyHeadquarters._2))
        Some((id, goal._1, goal._2))
      case Some(m) => Some(m)
    }
  }

  case class Building(btype: Int, x: Int, y: Int)


  abstract class Army {
    var gold = 0
    var income = 0
    var buildings = List.empty[Building]
    var units = List.empty[Soldier]
    val headquarters: (Int, Int)
    override def toString: String =
      s"gold/income = [$gold/$income]" +
        buildings.mkString("\nbuildings: ", ", ", "") +
        units.mkString("\nunits: ", ", ", "")
    def print = Console.err.println(toString)
  }


  class Me(world: World) extends Army {
    var lastCreated = Map.empty[(Int, Int), String]
    var unitidRole = Map.empty[Int, String]
    lazy val ehq = if (buildings.head.x == 0) 11 else 0
    override lazy val headquarters: (Int, Int) = (buildings.head.x, buildings.head.y)
    lazy val enemyHeadquarters: (Int, Int) = (ehq, ehq)
    lazy val board = Board(world.boardMatrix)
    lazy val hqGraph = if (ehq == 0) board.bottomDownBfs else board.topLeftBfs
    lazy val ehqGraph = if (ehq == 0) board.topLeftBfs else board.bottomDownBfs
    lazy val towerSquare = if (headquarters._1 == 0) 1 else 10
    def bestTSPositionIsEmpty = world.boardMatrix(towerSquare)(towerSquare) == 'O'
    var myGold = gold
    var myIncome = income
    val el = List.empty[Action]

    def canGenerate(x: Int, y: Int) = !unitsExist((x, y)) && !buildingExist((x, y))
    def canKill(myLevel: Int, enemyLevel: Int) = myLevel > enemyLevel || (myLevel == 3 && enemyLevel == 3)
    def exists(p: (Int, Int), pList: List[(Int, Int)]) = pList.exists(point => point._1 == p._1 && point._2 == p._2)
    def buildingExist(p: (Int, Int)) = exists(p, buildings.map(b => (b.x, b.y)))
    def spotExist(p: (Int, Int)) = exists(p, world.mineSpotsData.map(spot => (spot(0), spot(1))))
    def unitsExist(p: (Int, Int)) = exists(p, units.map(s => (s.x, s.y)))
    def hqcClosest = world.closestWithoutUnits(headquarters._1, headquarters._2, units, List('.', 'O'))
    def roleLessThan(role: String, limit: Int) = unitidRole.values.toList.count(_ == role) < limit
    def levelLessThan(lvl: Int, limit: Int) = units.count(_.level == 1) < limit
    def turnTo(id: Int, newRole: String) = unitidRole += (id -> newRole)

    def born(unitid: Int, level: Int, world: World, x: Int, y: Int): Soldier = {
      unitidRole.getOrElse(unitid, "Unknown") match {
        case "Guardian" => new Guardian(unitid, level, world, x, y, this)
        case "Conqueror" => new Conqueror(unitid, level, world, x, y, this)
        case "Scout" => new Scout(unitid, level, world, x, y, this)
        case "Unknown" =>
          unitidRole += (unitid -> lastCreated.getOrElse((x, y), "Conqueror"))
          born(unitid, level, world, x, y)
      }
    }

    def needTower = {
      val gold = myGold > 15
      val bp = bestTSPositionIsEmpty
      Console.err.println(s"needTower-------------------------------------------------")
      Console.err.println(s"myGold: $myGold")
      Console.err.println(s"towerSquare=$towerSquare")
      Console.err.println(s"bp: $bp")
      gold && bp &&
        (world.closestEnemySoldierTo2(headquarters._1, headquarters._2, hqGraph._2, board) match {
          case Some((unit, distance)) => {
            Console.err.println(s"enemy: $unit $distance")
            distance < 5
          }
          case None => {
            Console.err.println(s"nt::FALSE")
            false
          }
        }) && !buildingExist((towerSquare, towerSquare)) && !spotExist((towerSquare, towerSquare))
    }

    def emptyUnits = {
      val points = hqcClosest
      if (units.isEmpty)
        if (points.isEmpty) {
          List(Wait("FUCK =("))
        } else {
          points.map { hqc => {
            lastCreated = lastCreated ++ List(hqc -> "Conqueror")
            myGold -= 10
            Train(1, hqc._1, hqc._2)
          }
          }
        } else el
    }

    def tower = {
      val nt = needTower
      Console.err.println(s"needTower: $nt")
      if (needTower) {
        myGold -= 15
        List(Build("TOWER", towerSquare, towerSquare))
      } else el
    }

    def mine = {
      if (myGold > 50) {
        val spots = world.mineSpotsData.filter(spot => world.boardMatrix(spot(1))(spot(0)) == 'O'
          && !buildings.exists(b => b.x == spot(0) && b.y == spot(1))
          && !units.exists(unit => unit.x == spot(0) && unit.y == spot(1))
        )
        val spotToMine = world.closestEuclidean((headquarters._1, headquarters._2), spots.map(arr => (arr(0), arr(1))))
        if (spotToMine != headquarters) {
          // FIXME: не плодить мины, размножаться вначале
          myGold = myGold - 20 + 4 * world.mineSpotsData.size
          myIncome += 4
          List(Build("MINE", spotToMine._1, spotToMine._2))
        } else el
      } else el
    }

    def zergRush = {
      val lst = world.allWith(List('.', 'O'),
        (x, y) => !unitsExist((x, y)) && !buildingExist((x, y)) &&
          world.closest(x, y, List('O')).nonEmpty
      )
      if (lst.isEmpty) List(Wait("ZERG RUSH!")) else el
    }

    def train = {
      // FIXME: считать точно куда могу размножиться, с учётом вражеских юнитов
      val adjES = world.enemySoldiers.withFilter(es =>
        world.adjTo(es.x, es.y, 'O') && es.level < 3).map(es => ((es.x, es.y), es )).toMap
      val generable = world.allWith(List('.', 'O', 'x'), (x, y) => world.closest(x, y, List('O')).nonEmpty
        && canGenerate(x, y)) ++ adjES.keys
      val aa = world.all(List('X')).map(p => (p, world.closest(p._1, p._2, List('X')).size)).filter(rec => rec._2 == 2).toMap

      var sortedGenerable = generable.sortBy(p =>
        //      if (world.boardMatrix(p._2)(p._1) == 'O') 1.2 else 1 *
        if (aa.get(p).isDefined) 0.5 else 1 *
          ehqGraph._2(board.toNumber((p._1, p._2))) )
      //        * world.euclidean(p, (enemyHeadquarters._1, enemyHeadquarters._2)))
      var actionList = el

      def generate(p: (Int, Int), lvl: Int) = {
        if (lvl != 1 || (lvl == 1 && levelLessThan(1, 25))) {
          lastCreated = lastCreated + ((p._1, p._2) -> "Scout" /*(if (lvl == 1) "Conqueror" else  "Scout")*/)
          myIncome = if (world.boardMatrix(p._2)(p._1) == 'O') myIncome else myIncome + 1
          myIncome = if (lvl == 3) myIncome - 20 else if (lvl == 2) myIncome - 4 else myIncome - 1
          myGold = myGold - (lvl * 10)
          Train(lvl, p._1, p._2) :: actionList
        } else actionList
      }

      while (myGold > 10 && myIncome > 0) {
        actionList = sortedGenerable.headOption match {
          case Some(h) =>
            // FIXME: считать точно золото
            sortedGenerable = sortedGenerable.tail
            val lvl = if (myGold > 40) if (myGold > 100) 3 else 2 else 1
            adjES.get((h._1, h._2)) match {
              case Some(unit) => if (canKill(lvl, unit.level)) {
                generate(h, lvl)
              } else {
                sortedGenerable = if (sortedGenerable.nonEmpty) sortedGenerable.tail else sortedGenerable
                el
              }
              case None => generate(h, lvl)
            }
          case None => {
            myGold = 0
            actionList
          }
        }
      }
      actionList
    }

    def getActions: Option[List[Action]] = {
      var actionList = el



      actionList ++=
        emptyUnits ++
          tower ++
          mine ++
          zergRush ++
          train

      Some(actionList)
    }

    def nextAction(enemy: Enemy): List[Action] = {
      getActions match {
        case Some(train) => if (units.isEmpty) train else train ++ List(Move(this))
        case None => List(Move(this))
      }
    }

    override def toString: String = "ME-------------------------------------------------------------\n" + super.toString

    def closest(unit: Soldier, world: World) = world.closest(unit.x, unit.y, List('.'))

    def update(world: World) = {
      val deadUnitsId = unitidRole.keys.filterNot(key => units.exists(key == _.id))
      deadUnitsId.foreach(key => unitidRole = unitidRole - key)
      myGold = gold
      myIncome = income
    }
  }

  class Enemy extends Army {
    override lazy val headquarters: (Int, Int) = (buildings.head.x, buildings.head.y)
    override def toString: String = "ENEMY----------------------------------------------------------\n" + super.toString
  }


  sealed abstract class Action {
    def str: String
  }
  case class Wait(message: String) extends Action {
    override def str: String = s"WAIT; MSG $message"
  }
  case class Train(level: Int, x: Int, y: Int) extends Action {
    override def str: String = s"TRAIN $level $x $y"
  }

  case class Build(buildType: String, x: Int, y: Int) extends Action {
    override def str: String = s"BUILD $buildType $x $y"
  }

  case class Move(me: Me) extends Action {
    val unitsMoves = me.units.map(_.move)

    override def str: String = unitsMoves.map(m => s" MOVE ${m.get._1} ${m.get._2} ${m.get._3}").reduce(_ + "; " + _)
  }
  //------------------------------------------ENTERS----------------------------------------------------------------------
  val numberminespots = readInt
  val nms = (for (i <- 0 until numberminespots) yield {
    for (i <- readLine split " ") yield i.toInt
  }).toList

  if (outputLikeInput) Console.err.println(s"$numberminespots")
  if (outputLikeInput) nms.foreach(arr => Console.err.println(s"${arr(0)} ${arr(1)}"))

  val world = World(numberminespots, nms)
  val enemy = new Enemy
  val me = new Me(world)

  // game loop
  while (if (isTest) step < limit else true) {
    val gold = readInt
    if (outputLikeInput) Console.err.println(s"$gold")
    me.gold = gold
    val income = readInt
    if (outputLikeInput) Console.err.println(s"$income")
    me.income = income
    val opponentgold = readInt
    if (outputLikeInput) Console.err.println(s"$opponentgold")
    enemy.gold = opponentgold
    val opponentincome = readInt
    if (outputLikeInput) Console.err.println(s"$opponentincome")
    enemy.income = opponentincome
    world.board = (for (i <- 0 until 12) yield readLine).toArray
    world.boardMatrix = world.board.map(_.toCharArray)
    if (outputLikeInput) world.printBoard

    val buildingcount = readInt
    if (outputLikeInput) Console.err.println(s"$buildingcount")
    world.buildingcount = buildingcount

    me.buildings = List.empty
    enemy.buildings = List.empty

    for (i <- 0 until buildingcount) {
      val Array(owner, buildingtype, x, y) = for (i <- readLine split " ") yield i.toInt
      if (outputLikeInput) Console.err.println(s"$owner $buildingtype $x $y")
      val player = if (owner == 0) me else enemy
      player.buildings = Building(buildingtype, x, y) :: player.buildings
    }

    me.units = List.empty
    enemy.units = List.empty

    val unitcount = readInt
    if (outputLikeInput) Console.err.println(s"$unitcount")
    world.unitcount = unitcount
    for (i <- 0 until unitcount) {
      val Array(owner, unitid, level, x, y) = for (i <- readLine split " ") yield i.toInt
      if (outputLikeInput) Console.err.println(s"$owner $unitid $level $x $y")
      if (owner == 0) me.units = me.born(unitid, level, world, x, y) :: me.units
      else enemy.units = Soldier(unitid, level, world, x, y) :: enemy.units
      world.mySoldiers = me.units
      world.enemySoldiers = enemy.units
    }

    me.update(world)

    //------------------------------------------ACTIONS---------------------------------------------------------------------

    if (output) {
      world.print
      me.print
      enemy.print
    }

    val action = if (isDebug) List(Wait("Test")) else me.nextAction(enemy)


    step += 1
    println(s"${action.map(_.str).reduce(_ + "; " + _)}")
    //    Console.err.println(board.bfs(0))
  }
}
