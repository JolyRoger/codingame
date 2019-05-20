package codingames.challenge.iceandfire.src.army

import codingames.challenge.iceandfire.src.{Action, Move, Train, World}

class Me(world: World, enemy: Enemy) extends Army {
  def born(unitid: Int, level: Int, world: World, x: Int, y: Int): Soldier = {
    unitidRole.getOrElse(unitid, "Unknown") match {
      case "Guardian" => new Guardian(unitid, level, world, x, y, headquarters)
      case "Conqueror" => new Conqueror(unitid, level, world, x, y)
      case "Scout" => new Scout(unitid, level, world, x, y, enemyHeadquarters)
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
    lastCreated = "Scout"
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