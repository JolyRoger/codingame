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

  val debug = false

  val action = List(
    Map(2 -> "MOVE 2 10", 1 -> "MOVE 12 6"),
    Map(2 -> "MOVE 1 10", 1 -> "MOVE 12 6"),
    Map(2 -> "MOVE 2 10", 1 -> "MOVE 12 6"),
    Map(2 -> "MOVE 1 10", 1 ->  "MOVE 12 6"),
    Map(2 -> "MOVE 2 10", 1 ->  "MOVE 12 6; THROW 12 2"),
    Map(2 -> "MOVE 1 10", 1 ->  "MOVE 12 5; THROW 12 9"),
    Map(2 -> "MOVE 2 10", 1 ->  "MOVE 6 2"),
    Map(2 -> "MOVE 1 10", 1 ->  "MOVE 6 2"),
    Map(2 -> "MOVE 2 10", 1 -> "MOVE 6 2"),
    Map(2 -> "MOVE 1 10", 1 -> "MOVE 6 2"),
    Map(2 -> "MOVE 2 10", 1 -> "MOVE 6 2"),
    Map(2 -> "MOVE 1 10", 1 -> "MOVE 6 2"),
    Map(2 -> "MOVE 2 10", 1 -> "MOVE 6 2"),
    Map(2 -> "MOVE 1 10", 1 -> "MOVE 6 2"),
    Map(2 -> "MOVE 1 10", 1 -> "MOVE 6 2"),
    Map(2 -> "MOVE 1 10", 1 -> "MOVE 6 2"),
    Map(2 -> "MOVE 2 10; THROW 2 9", 1 -> "MOVE 6 2; THROW 2 2")
  )

  case class Point(x: Int, y: Int) {
    override def toString: String = s"$x $y"
  }
  case class Tile(id: Int, x: Int, y: Int, tileType: Int) {

    private def toTiles(coords: Set[(Int, Int)]) = coords.collect {
      case (_x, _y) if _x >= 0 && _x < width && _y >= 0 && _y < height => tiles(_y * width + _x)
    }.filter(_.tileType == 0)

    def calculateDistance(p: Point) = manhattanDistance(this.p, p)
    def calculateRating(agent: Int) = {
      val distancePower = maxManhattanDistance - distanceMap(agent)
      val distanceWeight = 2
      val powerWeight = 1
      val rating = (distancePower * distanceWeight + power * powerWeight) / (distanceWeight * powerWeight)
      rating
    }

    var power = 0
    val p = Point(x, y)
    var defendMap = Map.empty[Int, Int]
    var distanceMap = Map.empty[Int, Int]
    val updatePower = () => power = defendMap.values.map(damageCalc).sum


    lazy val adjs = toTiles(Set((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)))
    lazy val leftDanger = toTiles(Set((x - 1, y), (x - 1, y - 1), (x - 1, y + 1))).map(_.id)
    lazy val rightDanger = toTiles(Set((x + 1, y), (x + 1, y - 1), (x + 1, y + 1))).map(_.id)
    lazy val upDanger = toTiles(Set((x, y - 1), (x + 1, y - 1), (x - 1, y - 1))).map(_.id)
    lazy val downDanger = toTiles(Set((x, y + 1), (x + 1, y + 1), (x - 1, y + 1))).map(_.id)
  }

  case class Agent(id: Int, mine: Boolean, shootCooldown: Int, optimalRange: Int, soakingPower: Int, var splashBombs: Int) {
    var x: Int = _
    var y: Int = _
    var cooldown: Int = _
    var wetness: Int = _

    def p = Point(x, y)
    def tile = tiles(y * width + x)
    var target = List.empty[String]
    def print = s"$id; ${target.mkString(";")}"
  }

  val damageCalc = Array(0, 2, 3)

  val targets = List(Point(6, 1), Point(6, 3))

  val myId = readLine.toInt // Your player id (0 or 1)
  if (debug) Console.err.println(s"$myId")
  private val agentCount = readLine.toInt // Total number of agents in the game
  if (debug) Console.err.println(s"$agentCount")



  var agents = (0 until agentCount).map { _ =>
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

  var (myAgents, enemyAgents) = agents.partition(_.mine)
  val agentMap = agents.map(agent => (agent.id, agent)).toMap

  // width: Width of the game map
  // height: Height of the game map
  val Array(width, height) = (readLine split " ").filter(_ != "").map (_.toInt)

  val maxManhattanDistance = width + height

  if (debug) Console.err.println(s"$width $height")

  val tiles = Array.ofDim[Tile](width * height)
  for (_ <- 0 until height) {
    val inputs = readLine split "\\s+"
    if (debug) Console.err.println(s"${inputs.mkString(" ")}")

    for (j <- 0 until width) {
      // x: X coordinate, 0 is left edge
      // y: Y coordinate, 0 is top edge
      val x = inputs(3 * j).toInt
      val y = inputs(3 * j + 1).toInt
      val tileType = inputs(3 * j + 2).toInt
      val id = y * width + x
      tiles(id) = Tile(y * width + x, x, y, tileType)
    }
  }

  val covers = tiles.filter(tile => tile.tileType != 0)
  val shelters = covers.flatMap(_.adjs)

  def squares(coord: Tile => Int) = covers.map(coord).groupBy(identity).view
        .mapValues(_.length).toMap
        .groupBy { case (_, count) => count }       // Group by count
        .view
        .mapValues(_.keys.toList).toMap
        .maxBy(_._1)._2

  def manhattanDistance(p1: Point, p2: Point) = Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y)

  def defend(agent: Agent, shelter: Tile, cover: Tile) = {
    val agentTile = tiles(agent.y * width + agent.x)

    val res = if (shelter.x > cover.x) {
      if (agent.x < cover.x && !cover.leftDanger.contains(agentTile.id)) cover.tileType else 0
    } else if (shelter.x < cover.x) {
      if (agent.x > cover.x && !cover.rightDanger.contains(agentTile.id)) cover.tileType else 0
    } else if (shelter.y < cover.y) {
      if (agent.y > cover.y && !cover.downDanger.contains(agentTile.id)) cover.tileType else 0
    } else if (shelter.y > cover.y) {
      if (agent.y < cover.y && !cover.upDanger.contains(agentTile.id)) cover.tileType else 0
    } else 0
    res
  }

  case class Strategy(outsideAgent: Agent, insideAgent: Agent, var moveTargets: List[Point],
                      var throwFirstOutsideTargets: Set[Point], insideAgentTarget: Point, lastOutsideTarget: Point)

  def calculateStrategy() = {
    val xs = squares(tile => tile.x).sorted
    val ys = squares(tile => tile.y).sorted

    val (x1Mid, x2Mid) = (xs.head + (xs(1) - xs.head) / 2, xs(2) + (xs(3) - xs(2)) / 2)
    val (y1Mid, y2Mid) = (ys.head + (ys(1) - ys.head) / 2, ys(2) + (ys(3) - ys(2)) / 2)
    val p1 = Point(x1Mid, y1Mid)
    val p2 = Point(x1Mid, y2Mid)
    val p3 = Point(x2Mid, y1Mid)
    val p4 = Point(x2Mid, y2Mid)

    val xTarget = x1Mid + (x2Mid - x1Mid) / 2
    val yTarget = y1Mid + (y2Mid - y1Mid) / 2
    val p1Target = Point(xTarget, y1Mid)
    val p2Target = Point(xTarget, y2Mid)
    val p3Target = Point(x1Mid, yTarget)
    val p4Target = Point(x2Mid, yTarget)

    val plist = Set(p1, p2, p3, p4)
    val pTargetList = List(p1Target, p2Target, p3Target, p4Target)

    val minDist = pTargetList.flatMap(target => plist.map(pp => manhattanDistance(pp, target))).min

    val insideAgent = myAgents.minBy(_.splashBombs)
    val outsideAgent = myAgents.maxBy(_.splashBombs)

    var moveTargets = pTargetList.filter(target => plist.exists(pp => manhattanDistance(pp, target) == minDist))
      .sortBy(target => manhattanDistance(insideAgent.p, target))(Ordering.Int.reverse)
    //    val closestTarget = closestTargets.maxBy(target => manhattanDistance(insideAgent.p, target))
    var throwFirstOutsideTargets = plist.filter(p => p.x == moveTargets.head.x || p.y == moveTargets.head.y)
    val insideAgentTarget = plist.minBy(p => manhattanDistance(p, insideAgent.p))
    val lastOutsideTarget = plist.find(p => p != insideAgentTarget && !throwFirstOutsideTargets.contains(p)).get

    Strategy(outsideAgent, insideAgent, moveTargets, throwFirstOutsideTargets, insideAgentTarget, lastOutsideTarget)
  }

  val updateDistance = () => {
    myAgents.foreach { agent =>
      shelters.foreach { shelter =>
        val distance = shelter.calculateDistance(agent.p)
        shelter.distanceMap += (agent.id -> distance)
      }
    }
  }

  val updateCover = () => {
    covers.foreach { cover =>
      cover.adjs.foreach { adj =>
        agents.foreach { agent =>
          val newDefend = defend(agent, adj, cover)
          adj.defendMap.get(agent.id) match {
            case Some(t) => if (newDefend > t) adj.defendMap += (agent.id -> newDefend)
            case None => adj.defendMap += (agent.id -> newDefend)
          }
        }
      }
    }
  }

  val findBest = () => shelters.sortBy(shelter => shelter.power)(Ordering.Int.reverse)
  val findClosest = (agent: Agent) => shelters.sortBy(_.calculateDistance(agent.p))

  lazy val strategy = calculateStrategy()

// -------------------------------------------- game loop --------------------------------------------------------------
  var i = 0

  while(true) {
    val agentCount = readLine.toInt
    if (debug) Console.err.println(s"$agentCount")

    val liveAgents = for(_ <- 0 until agentCount) yield {
      // cooldown: Number of turns before this agent can shoot
      // wetness: Damage (0-100) this agent has taken
      val Array(agentId, x, y, cooldown, splashBombs, wetness) = (readLine split " ").filter(_ != "").map (_.toInt)

//      if (debug) Console.err.println(s"AGENT FOUND! $agentId")

      val agent = agentMap(agentId)
      agent.x = x
      agent.y = y
      agent.cooldown = cooldown
      agent.splashBombs = splashBombs
      agent.wetness = wetness

      if (debug) Console.err.println(s"$agentId $x $y $cooldown $splashBombs $wetness")
      agentId
    }
    agents = agents.filter(agent => liveAgents.contains(agent.id))
    val allAgents = agents.partition(_.mine)
    myAgents = allAgents._1
    enemyAgents = allAgents._2


    val myAgentCount = readLine.toInt // Number of alive agents controlled by you
    if (debug) Console.err.println(s"$myAgentCount")

    updateCover()
    updateDistance()

    shelters.foreach(_.updatePower())

    if (manhattanDistance(strategy.outsideAgent.p, strategy.moveTargets.head) > 1) {
      strategy.outsideAgent.target ::= s"MOVE ${strategy.moveTargets.head.x} ${strategy.moveTargets.head.y}"
      strategy.insideAgent.target ::= s"MESSAGE Вперёд, коллега!"
    } else if (strategy.throwFirstOutsideTargets.nonEmpty && manhattanDistance(strategy.outsideAgent.p, strategy.moveTargets.head) < 2) {
      strategy.outsideAgent.target ::= s"MOVE ${strategy.moveTargets.head.x} ${strategy.moveTargets.head.y}; THROW ${strategy.throwFirstOutsideTargets.head.x} ${strategy.throwFirstOutsideTargets.head.y}"
      if (strategy.outsideAgent.splashBombs < 2){
        strategy.insideAgent.target ::= s"THROW ${strategy.insideAgentTarget.x} ${strategy.insideAgentTarget.y}"
      } else strategy.insideAgent.target ::= "MESSAGE Разбомби их на фиг!"
      strategy.throwFirstOutsideTargets = strategy.throwFirstOutsideTargets.tail
      if (strategy.throwFirstOutsideTargets.isEmpty) {
        strategy.moveTargets = strategy.moveTargets.tail
        strategy.throwFirstOutsideTargets = Set(strategy.lastOutsideTarget)
      }
    } else {
      Console.err.println(s"WHAT?!")
    }

//    val sheltersBest = findBest()

/*
    myAgents.foreach { agent =>
      val bestShelter = shelters.maxBy(_.calculateRating(agent.id))
      val bestTarget = enemyAgents.filter(enemy => manhattanDistance(enemy.p, agent.p) <= agent.optimalRange)
                                  .minByOption(ea => ea.tile.defendMap.getOrElse(agent.id, 0))
      bestTarget.foreach(bt => agent.target ::= s"SHOOT ${bt.id}")
      agent.target ::= s"MOVE ${bestShelter.p}"
    }

    // Write an action using println
    // To debug: if (debug) Console.err.println("Debug messages...")
    // One line per agent: <agentId>;<action1;action2;...> actions are "MOVE x y | SHOOT id | THROW x y | HUNKER_DOWN | MESSAGE text"
*/
    myAgents.foreach { agent =>
//      println(s"${agent.id}; ${action(i)(agent.id)}")
      println(s"${agent.print}")
      agent.target = List.empty
    }

    shelters.foreach(_.defendMap = Map.empty)
    i += 1
  }
}