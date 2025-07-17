package codingames.challenge.solk

import math._
import scala.io.{Source, StdIn}
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
  
  val debug = true
  
  case class Point(x: Int, y: Int) {
    override def toString: String = s"$x $y"
  }
  case class Agent(id: Int, mine: Boolean, shootCooldown: Int, optimalRange: Int, soakingPower: Int, var splashBombs: Int) {
    var x: Int = _
    var y: Int = _
    var cooldown: Int = _
    var wetness: Int = _

    var target = List.empty[String]
    def print = s"$id; ${target.mkString(";")}"
  }

  val targets = List(Point(6, 1), Point(6, 3))

  val myId = readLine.toInt // Your player id (0 or 1)
  if (debug) Console.err.println(s"$myId")
  private val agentCount = readLine.toInt // Total number of agents in the game
  if (debug) Console.err.println(s"$agentCount")

  val agents = (0 until agentCount).map { _ =>
    // agentId: Unique identifier for this agent
    // player: Player id of this agent
    // shootCooldown: Number of turns between each of this agent's shots
    // optimalRange: Maximum manhattan distance for greatest damage output
    // soakingPower: Damage output within optimal conditions
    // splashBombs: Number of splash bombs this can throw this game
    val Array(agentId, player, shootCooldown, optimalRange, soakingPower, splashBombs) = (readLine split "\\s+").map(_.toInt)
    if (debug) Console.err.println(s"$agentId $player $shootCooldown $optimalRange $soakingPower $splashBombs")
    Agent(agentId, player == myId, shootCooldown, optimalRange, soakingPower, splashBombs)
  }

  private val (myAgents, enemyAgents) = agents.partition(_.mine)
  val agentMap = agents.map(agent => (agent.id, agent)).toMap

  // width: Width of the game map
  // height: Height of the game map
  val Array(width, height) = (readLine split " ").filter(_ != "").map (_.toInt)
  if (debug) Console.err.println(s"$width $height")

  for(i <- 0 until height) {
    var inputs = readLine split "\\s+"
    if (debug) Console.err.println(s"${inputs.mkString(" ")}")

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
    if (debug) Console.err.println(s"$agentCount")

    for(i <- 0 until agentCount) {
      // cooldown: Number of turns before this agent can shoot
      // wetness: Damage (0-100) this agent has taken
      val Array(agentId, x, y, cooldown, splashBombs, wetness) = (readLine split " ").filter(_ != "").map (_.toInt)
      val agent = agentMap(agentId)
      agent.x = x
      agent.y = y
      agent.cooldown = cooldown
      agent.splashBombs = splashBombs
      agent.wetness = wetness

      if (debug) Console.err.println(s"$agentId $x $y $cooldown $splashBombs $wetness")
    }

    val myAgentCount = readLine.toInt // Number of alive agents controlled by you
    if (debug) Console.err.println(s"$myAgentCount")

      // Write an action using println
      // To debug: if (debug) Console.err.println("Debug messages...")

    var taken = Set.empty[Int]
    myAgents.foreach { myAgent =>
      enemyAgents.filterNot(agent => taken.contains(agent.id))
                 .maxByOption(_.wetness)
                 .foreach(a => {
                   myAgent.target = s"SHOOT ${a.id}" :: myAgent.target
                   taken += a.id
                 })
    }
    taken = Set.empty[Int]

      // One line per agent: <agentId>;<action1;action2;...> actions are "MOVE x y | SHOOT id | THROW x y | HUNKER_DOWN | MESSAGE text"
    myAgents.foreach { agent =>
      println(s"${agent.print}")
      agent.target = List.empty
    }
  }
}