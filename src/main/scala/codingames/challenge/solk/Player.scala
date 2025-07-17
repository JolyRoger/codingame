package codingames.challenge.solk

import math._
import scala.io.Source
import scala.util._
import scala.io.StdIn._

/**
 * Win the water fight by controlling the most territory, or out-soak your opponent!
 **/
object Player extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
  val filename = "resources/solk/1.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines
  def readInt = if (data.hasNext) data.next.toInt else { System.exit(0); -1 }
  def readLine = if (data.hasNext) data.next else { System.exit(0); "" }
//----------------------------------------------------------------------------------------------------------------------
  case class Point(x: Int, y: Int)
  case class Agent(id: Int, player: Int, shootCooldown: Int, optimalRange: Int, soakingPower: Int, splashBombs: Int) {
    var target: Point = _
  }

  val myId = readLine.toInt // Your player id (0 or 1)
  Console.err.println(s"$myId")
  private val agentCount = readLine.toInt // Total number of agents in the game
  Console.err.println(s"$agentCount")

  val agents = (0 until agentCount).map { _ =>
    // agentId: Unique identifier for this agent
    // player: Player id of this agent
    // shootCooldown: Number of turns between each of this agent's shots
    // optimalRange: Maximum manhattan distance for greatest damage output
    // soakingPower: Damage output within optimal conditions
    // splashBombs: Number of splash bombs this can throw this game
    val Array(agentId, player, shootCooldown, optimalRange, soakingPower, splashBombs) = (readLine split "\\s+").map(_.toInt)
    Console.err.println(s"$agentId $player $shootCooldown $optimalRange $soakingPower $splashBombs")
    Agent(agentId, player, shootCooldown, optimalRange, soakingPower, splashBombs)
  }

  val (myAgents, enemyAgents) = agents.partition(_.player == myId)
  // width: Width of the game map
  // height: Height of the game map
  val Array(width, height) = (readLine split " ").filter(_ != "").map (_.toInt)
  Console.err.println(s"$width $height")

  for(i <- 0 until height) {
    var inputs = readLine split "\\s+"
    Console.err.println(s"${inputs.mkString(" ")}")

    for(j <- 0 until width) {
      // x: X coordinate, 0 is left edge
      // y: Y coordinate, 0 is top edge
      val x = inputs(3 * j).toInt
      val y = inputs(3 * j + 1).toInt
      val tileType = inputs(3 * j + 2).toInt
    }
  }

  // game loop
  while(true) {
    val agentCount = readLine.toInt
    Console.err.println(s"$agentCount")

    for(i <- 0 until agentCount) {
      // cooldown: Number of turns before this agent can shoot
      // wetness: Damage (0-100) this agent has taken
      val Array(agentId, x, y, cooldown, splashBombs, wetness) = (readLine split " ").filter(_ != "").map (_.toInt)
      Console.err.println(s"$agentId $x $y $cooldown $splashBombs $wetness")
    }

    val myAgentCount = readLine.toInt // Number of alive agents controlled by you
    Console.err.println(s"$myAgentCount")
    for(i <- 0 until myAgentCount) {

      // Write an action using println
      // To debug: Console.err.println("Debug messages...")


      // One line per agent: <agentId>;<action1;action2;...> actions are "MOVE x y | SHOOT id | THROW x y | HUNKER_DOWN | MESSAGE text"
      println("HUNKER_DOWN")
    }
  }
}