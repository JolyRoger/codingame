package codingames.challenge.iceandfire.src.army

import codingames.challenge.iceandfire.src.{Action, Move, Train, World}

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

  def trainCondition = units.isEmpty ||
                               (gold > 30 && income > 5) ||
                                units.size < 10

  def hqcClosest = world.closestWithoutUnits(headquarters._1, headquarters._2, units, List('.', 'O'))

  def roleLessThan(role: String, limit: Int) = unitidRole.values.toList.count(_ == role) < limit

  def getTrain = if (units.isEmpty) {
    val hqc = hqcClosest.head
    lastCreated = "Conqueror"
    Some(Train(1, hqc._1, hqc._2))
  } else if (gold > 50 && hqClosest.nonEmpty && roleLessThan("Guardian", 2)) {
    val hqc = hqcClosest.head
    lastCreated = "Guardian"
    Some(Train(2, hqc._1, hqc._2))
  } else if (income > 2 && gold > 10 && roleLessThan("Conqueror", 10)) {
    val chqc = world.myClosestTo(enemyHeadquarters)
    lastCreated = "Conqueror"
    Some(Train(1, chqc._1, chqc._2))
  } else if (income > 30 && gold > 400 && roleLessThan("Scout", 1)) {
    val chqc = world.myClosestTo(enemyHeadquarters)
    lastCreated = "Scout"
    Some(Train(3, chqc._1, chqc._2))
  }
  else None

  def nextAction(enemy: Enemy): List[Action] = {
    getTrain match {
      case Some(train) => if (units.isEmpty) List(train) else List(Move(this), train)
      case None => List(Move(this))
    }
  }

  override def toString: String = "ME-------------------------------------------------------------\n" + super.toString

  def closest(unit: Soldier, world: World) = world.closest(unit.x, unit.y, List('.'))

  def update(world: World) = {
    hqClosest = world.closestWithoutUnits(headquarters._1, headquarters._2, units, List('.', 'O'))
    val deadUnitsId = unitidRole.keys.filterNot(key => units.exists(key == _.id))
    deadUnitsId.foreach(key => unitidRole = unitidRole - key)
  }
}