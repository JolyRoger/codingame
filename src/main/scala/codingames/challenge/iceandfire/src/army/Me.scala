package codingames.challenge.iceandfire.src.army

import codingames.challenge.iceandfire.src._

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
  def bestTSPositionIsEmpty = world.boardMatrix(towerSquare)(towerSquare) == 'O' && world.doesNotContainMyUnit(towerSquare, towerSquare)
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
    myGold > 15 && bestTSPositionIsEmpty &&
      (world.closestEnemySoldierTo2(headquarters._1, headquarters._2, hqGraph._2, board) match {
        case Some((unit, distance)) => {
          distance < 5
        }
        case None => {
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
      val spotToMine = world.closestToHQ(headquarters, spots.map(arr => (arr(0), arr(1))), hqGraph._2, board)
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
      if (lvl != 1 || (lvl == 1 && levelLessThan(1, 30))) {
        lastCreated = lastCreated + ((p._1, p._2) -> (if (lvl == 1) "Conqueror" else  "Scout"))
        myIncome = if (world.boardMatrix(p._2)(p._1) == 'O') myIncome else myIncome + 1
        myIncome = if (lvl == 3) myIncome - 20 else if (lvl == 2) myIncome - 4 else myIncome - 1
        myGold = myGold - (lvl * 10)
        Train(lvl, p._1, p._2) :: actionList
      } else actionList
    }

    while (myGold > 0 && myIncome > 0) {
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