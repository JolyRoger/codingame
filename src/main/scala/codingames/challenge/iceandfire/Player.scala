//package codingames.challenge.iceandfire


object Player extends App {

//------------------------------------------VARIABLES-------------------------------------------------------------------
  val condition = true
  val isDebug = false
  val output = false
  val outputLikeInput = true
  var step = 0
  val limit = 1
  val isTest = false



//------------------------------------------CLASSES---------------------------------------------------------------------


case class World(numMineSpots: Int, mineSpotsData: List[Array[Int]]) {
//  def turnTo(id: Int, str: String) =

  // World class
  var buildingcount = 0
  var unitcount = 0
  var board = Array.empty[String]
  var boardMatrix: Array[Array[Char]] = Array.empty
  var mySoldiers: List[Soldier] = List.empty
  var enemySoldiers: List[Soldier] = List.empty

  def closestWithoutUnits(x: Int, y: Int, soldiers: List[Soldier], sym: List[Char]) = {
    closest(x, y, sym).filterNot(
      point => soldiers.exists(
        unit => unit.x == point._1 && unit.y == point._2))
  }

  def closestWithUnits(x: Int, y: Int, units: List[Soldier], sym: List[Char]) =
    closest(x, y, sym).withFilter(
      point => units.exists(unit => unit.x == point._1 && unit.y == point._2)
    ).map(point => (point, units.find(unit => unit.x == point._1 && unit.y == point._2).get))

  def closest(x: Int, y: Int, sym: List[Char]) = {
    val candidates = List((x-1, y), (x, y-1), (x+1, y), (x, y+1))
    candidates.filter(square =>
      square._1 >= 0 &&
        square._2 >= 0 &&
        square._1 < 12 &&
        square._2 < 12 &&
        sym.contains(boardMatrix(square._2)(square._1)))
  }
  def closestEmpty(x: Int, y: Int) = closest(x, y, List('.'))
  def closestFree(x: Int, y: Int) = closestWithoutUnits(x, y, mySoldiers, List('.', 'O'))
  def allFree(x: Int, y: Int) = (for (i <- 0 until 12; j <- 0 until 12; if boardMatrix(j)(i) == '.') yield (j, i)).toList

  def closestEnemy(x: Int, y: Int) = closest(x, y, List('X'))
  def closestEnemySoldiers(x: Int, y: Int) = closestWithUnits(x, y, enemySoldiers, List('X'))

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
      val goal = world.allFree(x, y).headOption.getOrElse((me.enemyHeadquarters._1, me.enemyHeadquarters._2))
      Some((id, goal._2, goal._1))
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
        unitidRole += (unitid -> lastCreated)
        born(unitid, level, world, x, y)
    }
  }

  var lastCreated: String = ""

  var unitidRole = Map.empty[Int, String]
  lazy val enemyHeadquarters: (Int, Int) = (enemy.buildings.head.x, enemy.buildings.head.y)
  override lazy val headquarters: (Int, Int) = (buildings.head.x, buildings.head.y)
  var hqClosest: List[(Int, Int)] = List.empty
  var trainLevel: Int = 0

  def trainCondition = units.isEmpty ||
                               (gold > 30 && income > 5) ||
                                units.size < 10

  def hqcClosest = world.closestWithoutUnits(headquarters._1, headquarters._2, units, List('.', 'O'))

  def getTrain = if (units.isEmpty) {
    val hqc = hqcClosest.head
    lastCreated = "Conqueror"
    Some(Train(1, hqc._1, hqc._2))
  } else if (gold > 50 && hqClosest.nonEmpty && unitidRole.values.toList.count(_ == "Guardian") < 2) {
    val hqc = hqcClosest.head
    lastCreated = "Guardian"
    Some(Train(2, hqc._1, hqc._2))
  } else None

  def nextAction(enemy: Enemy): Action =
    getTrain match {
      case Some(train) => train
      case None => Move(this)
    }

  override def toString: String = "ME-------------------------------------------------------------\n" + super.toString

  def closest(unit: Soldier, world: World) = world.closest(unit.x, unit.y, List('.'))

  def update(world: World) = {
    hqClosest = world.closestWithoutUnits(headquarters._1, headquarters._2, units, List('.', 'O'))
    trainLevel = if (gold < 50) 1 else 2
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
case class Move(me: Me) extends Action {
  val unitsMoves = me.units.map(_.move)

  override def str: String = unitsMoves.map(m => s" MOVE ${m.get._1} ${m.get._2} ${m.get._3};").reduce(_ + _)
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

    val action = if (isDebug) Wait("Test") else me.nextAction(enemy)


    step += 1
    println(s"${action.str}")
    Console.err.println
  }
}
