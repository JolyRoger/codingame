package codingames.challenge

import math._
import scala.io.Source
import scala.util._
import scala.io.StdIn._

object Player extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
  val filename = "resources/grass/1.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines
  def readInt = if (data.hasNext) data.next.toInt else { System.exit(0); -1 }
  def readLine = if (data.hasNext) data.next else { System.exit(0); "" }
//----------------------------------------------------------------------------------------------------------------------
  val Array(width, height) = (readLine split " ").filter(_ != "").map (_.toInt)

  case class Tile(id: Int, scrapAmount: Int, owner: Int, units: Int, hasRecycler: Boolean, canBuild: Boolean, canSpawn: Boolean, inRangeOfRecycler: Boolean)
  case class Robot(id: Int, pos: Int)

  for (_ <- LazyList.from(0).takeWhile(_ < 201)) {
    var tiles = List.empty[Tile]
    var myRobots = List.empty[Robot]
    var foeRobots = List.empty[Robot]

    val Array(myMatter, oppMatter) = (readLine split "\\s+").map(_.toInt)
    Console.err.println(s"$myMatter $oppMatter")
    var myRobotIndex = 0
    var foeRobotIndex = 0

    for(i <- 0 until height; j <- 0 until width) {
      val index = i * width + j
      val Array(scrapAmount, owner, units, recycler, canBuild, canSpawn, inRangeOfRecycler) = (readLine split " ").withFilter(_ != "").map (_.toInt)
      Console.err.println(s"$scrapAmount $owner $units $recycler $canBuild $canSpawn $inRangeOfRecycler")
      tiles = Tile(index, scrapAmount, owner, units, recycler == 1, canBuild == 1, canSpawn == 1, inRangeOfRecycler == 1) :: tiles
      if (units > 0) {
        for (_ <- 0 until units) {
          if (owner == 1) {
            myRobots = Robot(myRobotIndex, index) :: myRobots
            myRobotIndex += 1
          } else if (owner == 0) {
            foeRobots = Robot(foeRobotIndex, index) :: foeRobots
            foeRobotIndex += 1
          }
        }
      }
    }

    println("WAIT")
  }
}