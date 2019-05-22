package codingames.challenge.iceandfire.src

import codingames.challenge.iceandfire.src.army.Soldier

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