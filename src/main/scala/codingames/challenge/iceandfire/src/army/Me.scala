package codingames.challenge.iceandfire.src.army

import codingames.challenge.iceandfire.src.{Move, Train, World}

class Me(world: World, enemy: Enemy) extends Army {
  def born(role: String, unitid: Int, level: Int, world: World, x: Int, y: Int, headquarters: (Int, Int)) = {
    role match {
      case "Guardian" => new Guardian(unitid, level, world, x, y, headquarters)
      case _ => new Guardian(unitid, level, world, x, y, headquarters)
    }
  }

  // Me class
  override lazy val headquarters: (Int, Int) = (buildings.head.x, buildings.head.y)
  private var hqClosest: List[(Int, Int)] = List.empty
  private var trainLevel: Int = 0

  def checkForUnitCanHitEnemy(units: List[Soldier]) = {
    units.map(unit => {
      val listNeighbourEnemyUnits = world.closestWithUnits(unit.x, unit.y, enemy.units, List('X'))
      (unit, listNeighbourEnemyUnits)
    }).filter(_._2.nonEmpty).toMap
  }
  def trainCondition= units.isEmpty ||
    (gold > 50 && hqClosest.nonEmpty)

  def nextAction(enemy: Enemy) = {
    if (trainCondition) {
      val hqc = hqClosest.head
      Train(trainLevel, hqc._1, hqc._2)
    } else {
      val unitsCanHitEnemy = checkForUnitCanHitEnemy(units)
//      if (unitsCanHitEnemy.nonEmpty)
      //      val unitsMove = me.units.map(unit => me.closest(unit).orElse())
      val enemyHQ = enemy.headquarters
      Move(1, enemyHQ._1, enemyHQ._2)
    }
  }

  override def toString: String = "ME-------------------------------------------------------------\n" + super.toString

  def closest(unit: Soldier, world: World) = world.closest(unit.x, unit.y, List('.'))

  def update(world: World) = {
    hqClosest = world.closestWithoutUnits(headquarters._1, headquarters._2, units, List('.', 'O'))
    trainLevel = if (gold < 50) 1 else 2
  }
}