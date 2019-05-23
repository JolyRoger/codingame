package codingames.challenge.iceandfire.src.army

import codingames.challenge.iceandfire.src._

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