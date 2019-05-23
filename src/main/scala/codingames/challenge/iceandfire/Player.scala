//package codingames.challenge.iceandfire

import scala.math.{pow, sqrt}

object Player extends App {

//------------------------------------------VARIABLES-------------------------------------------------------------------
  val condition = true
  val isDebug = false
  val output = false
  val outputLikeInput = false
  var step = 0
  val limit = 1
  val isTest = false



//------------------------------------------CLASSES---------------------------------------------------------------------


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

  def closestWithoutUnits(x: Int, y: Int, soldiers: List[Soldier], sym: List[Char]) = closest(x, y, sym).filterNot(point => soldiers.exists(unit => unit.x == point._1 && unit.y == point._2))
  def closestWithUnits(x: Int, y: Int, units: List[Soldier], sym: List[Char]) =
    closest(x, y, sym).withFilter(
      point => units.exists(unit => unit.x == point._1 && unit.y == point._2)
    ).map(point => (point, units.find(unit => unit.x == point._1 && unit.y == point._2).get))

  def closest(x: Int, y: Int, sym: List[Char]) = List((x-1, y), (x, y-1), (x+1, y), (x, y+1)).filter(
    square => square._1 >= 0 && square._2 >= 0 && square._1 < 12 && square._2 < 12 &&
      sym.contains(boardMatrix(square._2)(square._1)))
  def closestEmpty(x: Int, y: Int) = closest(x, y, List('.'))
  def closestForConqueror(x: Int, y: Int) = closestWithoutUnits(x, y, enemySoldiers, List('.', 'X'))
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
  def all(x: Int, y: Int, sym: List[Char]) = (for (i <- 0 until 12; j <- 0 until 12; if sym.contains(boardMatrix(i)(j))) yield (j, i)).toList
  def allWith(sym: List[Char], filter: (Int, Int) => Boolean) = (for (i <- 0 until 12; j <- 0 until 12; if sym.contains(boardMatrix(i)(j)) && filter(j, i)) yield (j, i)).toList
  def allFree = allWith(List('.', 'O'), doesNotContainMyUnit)
  def allConquerorTarget = allWith(List('.', 'X'), doesNotContainEnemyUnit)
  def freeToMove(x: Int, y: Int) = (for (i <- 0 until 12; j <- 0 until 12; if boardMatrix(j)(i) == '.' || (boardMatrix(j)(i) == 'X' && !enemySoldiers.exists(soldier => i == soldier.x && j == soldier.y) )) yield (j, i)).toList

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


class Me(world: World, enemy: Enemy) extends Army {
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

//  var lastCreated: String = ""
  var lastCreated = Map.empty[(Int, Int), String]

  var unitidRole = Map.empty[Int, String]
  lazy val enemyHeadquarters: (Int, Int) = (enemy.buildings.head.x, enemy.buildings.head.y)
  override lazy val headquarters: (Int, Int) = (buildings.head.x, buildings.head.y)
  var hqClosest: List[(Int, Int)] = List.empty

  def trainCondition = units.isEmpty ||
                               (gold > 30 && income > 5) ||
                                units.size < 10

  lazy val towerSquare = if (headquarters._1 == 0) 1 else 10
  def hqcClosest = world.closestWithoutUnits(headquarters._1, headquarters._2, units, List('.', 'O'))

  def roleLessThan(role: String, limit: Int) = unitidRole.values.toList.count(_ == role) < limit

  def getTrain: Option[List[Action]] = {
    var trainList = List.empty[Action]

    if (units.isEmpty) {
      val hqc = hqcClosest.head
      lastCreated = lastCreated ++ List((hqc._1, hqc._2) -> "Scout", (hqc._2, hqc._1) -> "Scout")
      trainList = trainList ++ List(Train(1, hqc._1, hqc._2), Train(1, hqc._2, hqc._1))
    } else if (gold > 20 && world.boardMatrix(towerSquare)(towerSquare) == 'O'
       && !buildings.exists(building => building.x == towerSquare && building.y == towerSquare)
       && !world.mineSpotsData.exists(spot => spot(0) == towerSquare && spot(1) == towerSquare)) {
      trainList ::= Build("TOWER", towerSquare, towerSquare)
    } else {
      val lst = world.allWith(List('.', 'O'),
        (x, y) => !units.exists(unit => unit.x == x && unit.y == y) &&
        !buildings.exists(building => building.x == x && building.y == y) &&
        world.closest(x, y, List('O')).nonEmpty
      )
//      Console.err.println(s"${lst.mkString("[", ", ", "]")}")
      if (lst.isEmpty) {
        trainList ::= Wait("ZERG RUSH!")
    } else {
        var myGold = gold
        if (myGold > 20) {
          val spots = world.mineSpotsData.filter(spot => world.boardMatrix(spot(1))(spot(0)) == 'O' && !buildings.exists(b => b.x == spot(0) && b.y == spot(1)))
          val spotToMine = world.closestEuclidean((headquarters._1, headquarters._2), spots.map(arr => (arr(0), arr(1))))
          if (spotToMine != headquarters) {
            myGold = myGold - 20
            trainList ::= Build("MINE", spotToMine._1, spotToMine._2)
          }
        }

        val generable = world.allWith(List('.', 'O'), (x, y) => world.closest(x, y, List('O')).nonEmpty)
        var sortedGenerable = generable.sortBy(p => world.euclidean(p, (enemyHeadquarters._1, enemyHeadquarters._2)))

        while (myGold > 11) {
          trainList = sortedGenerable.headOption match {
            case Some(h) => {
              myGold = myGold - 10
              sortedGenerable = sortedGenerable.tail
              lastCreated = lastCreated + ((h._1, h._2) -> /*(if ((h._1 + h._2) % 2 == 0) "Scout" else */"Conqueror")
              Train(if (myGold > 50) 2 else 1, h._1, h._2) :: trainList
            }
            case None => trainList
          }
        }
      }
    }

    Some(trainList)
  }

/*
  def getTrain: Option[List[Action]] = if (units.isEmpty) {
    val hqc = hqcClosest.head
    lastCreated = "Conqueror"
    Some(List(Train(1, hqc._1, hqc._2), Train(1, hqc._2, hqc._1)))
  } else if (gold > 50 && hqClosest.nonEmpty && roleLessThan("Guardian", 2)) {
    val hqc = hqcClosest.head
    lastCreated = "Guardian"
    Some(List(Train(2, hqc._1, hqc._2)))
  } else if (income > 20 && gold > 20 && roleLessThan("Conqueror", 50)) {
    val chqc = world.myClosestTo(enemyHeadquarters)
    lastCreated = "Conqueror"
    Some(List(Train(2, chqc._1, chqc._2)))
  } else if (income > 2 && gold > 10 && roleLessThan("Conqueror", 50)) {
    val chqc = world.myClosestTo(enemyHeadquarters)
    lastCreated = "Conqueror"
    Some(List(Train(1, chqc._1, chqc._2)))
  } else if (income > 30 && gold > 400 && roleLessThan("Scout", 1)) {
    val chqc = world.myClosestTo(enemyHeadquarters)
    lastCreated = "Scout"
    Some(List(Train(3, chqc._1, chqc._2)))
  }
  else None
*/

  def nextAction(enemy: Enemy): List[Action] = {
    getTrain match {
      case Some(train) => if (units.isEmpty) train else List(Move(this)) ++ train
      case None => List(Move(this))
    }
  }

  override def toString: String = "ME-------------------------------------------------------------\n" + super.toString

  def closest(unit: Soldier, world: World) = world.closest(unit.x, unit.y, List('.'))

  def update(world: World) = {
//    lastCreated.foreach(lc => Console.err.println(s"(${lc._1._1}, ${lc._1._2} -> ${lc._2}"))
//    unitidRole.foreach(unitid => Console.err.println(s"(${unitid._1} => ${unitid._2}"))
    hqClosest = world.closestWithoutUnits(headquarters._1, headquarters._2, units, List('.', 'O'))
    val deadUnitsId = unitidRole.keys.filterNot(key => units.exists(key == _.id))
    deadUnitsId.foreach(key => unitidRole = unitidRole - key)
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
  val me = new Me(world, enemy)

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
    Console.err.println
  }
}
