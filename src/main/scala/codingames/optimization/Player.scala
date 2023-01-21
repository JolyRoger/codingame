package codingames.optimization

import math._
import scala.io.Source
import scala.util._
import scala.io.StdIn._

/**
 * Save humans, destroy zombies!
 **/
object Player extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
  val filename = "resources/zombie/19.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines
  def readInt = if (data.hasNext) data.next.toInt else { System.exit(0); -1 }
  def readLine = if (data.hasNext) data.next else { System.exit(0); "" }
//----------------------------------------------------------------------------------------------------------------------

  def euclidean(a: (Int, Int), b: (Int, Int)) = sqrt(pow(b._1 - a._1, 2) + pow(b._2 - a._2, 2))
  def findClosest(me: (Int, Int), characters: IndexedSeq[Character]) = characters.minBy(z => euclidean(me, z.point))
  def canSave(me: (Int, Int), human: Character, zombie: Character) = {
    val meZombieDistance = euclidean(me, zombie.point)
    val humanZombieDistance = euclidean(human.point, zombie.point)
    val myMoves = (meZombieDistance - 2000) / 1000
    val zombieMoves = humanZombieDistance / 400
    myMoves < zombieMoves
  }

  trait Character {
    def point: (Int, Int)
    def x = point._1
    def y = point._2
  }
  case class Human(humanId: Int, humanX: Int, humanY: Int) extends Character {
    val point = (humanX, humanY)
  }
  case class Zombie(zombieId: Int, zombieX: Int, zombieY: Int, zombieXNext: Int, zombieYNext: Int) extends Character {
    val point = (zombieX, zombieY)
  }

  var (targetX, targetY) = (0, 0)

  // game loop
  while(true) {
    val Array(x, y) = (readLine split " ").filter(_ != "").map (_.toInt)
//    Console.err.println(s"x=$x y=$y")
    val humanCount = readLine.toInt
//    Console.err.println(s"humanCount=$humanCount")
    val humans = for(i <- 0 until humanCount) yield {
      val Array(humanId, humanX, humanY) = (readLine split " ").filter(_ != "").map (_.toInt)
//      Console.err.println(s"humanId=$humanId, humanX=$humanX, humanY=$humanY")
      Human(humanId, humanX, humanY)
    }
    val zombieCount = readLine.toInt
//    Console.err.println(s"humanCount=$humanCount")

    val zombies = for(i <- 0 until zombieCount) yield {
      val Array(zombieId, zombieX, zombieY, zombieXNext, zombieYNext) = (readLine split " ").filter(_ != "").map (_.toInt)
//      Console.err.println(s"zombieId=$zombieId, zombieX=$zombieX, zombieY=$zombieY zombieXNext=$zombieXNext, zombieYNext=$zombieYNext")
      Zombie(zombieId, zombieX, zombieY, zombieXNext, zombieYNext)
    }

    val humanZombie = humans.map(human => (human, findClosest(human.point, zombies)))
    val saveableHumans = humanZombie.withFilter(hz => canSave((x, y), hz._1, hz._2)).map(_._1)
    val closestHuman = findClosest((x, y), saveableHumans)
    val closestZombie = findClosest(closestHuman.point, zombies)

    targetX = closestZombie.x
    targetY = closestZombie.y

    println(s"$targetX $targetY")
  }
}