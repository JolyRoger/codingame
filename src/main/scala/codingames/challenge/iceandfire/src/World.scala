package codingames.challenge.iceandfire.src

import codingames.challenge.iceandfire.src.army.Soldier

case class World(numMineSpots: Int, mineSpotsData: List[Array[Int]]) {                                                        // World class
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
  def closestFree(x: Int, y: Int) = closestWithoutUnits(x, y, mySoldiers ++ enemySoldiers, List('.', 'O'))
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

