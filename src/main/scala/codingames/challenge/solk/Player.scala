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

  trait HasAdjacent {
    def toPoints(coords: Set[(Int, Int)]): Set[Point] = coords.collect {
      case (_x, _y) if _x >= 0 && _x < width && _y >= 0 && _y < height => Point(_x, _y)
    }
  }

  case class Point(x: Int, y: Int) extends HasAdjacent {
    lazy val id = y * width + x
    def adjPoints = toPoints(Set((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1), (x, y),
      (x + 1, y + 1), (x - 1, y - 1), (x - 1, y + 1), (x + 1, y - 1)))
    override def toString: String = s"$x $y"
  }

  case class Tile(id: Int, x: Int, y: Int, tileType: Int) {
    private def toTiles(coords: Set[(Int, Int)]) = coords.collect {
      case (_x, _y) if _x >= 0 && _x < width && _y >= 0 && _y < height => tiles(_y * width + _x)
    }.filter(_.tileType == 0)

    def calculateDistance(p: Point) = manhattanDistance(this.p, p)

    def calculateRating(agent: Int) = {
      val distancePower = maxManhattanDistance - shelterDistanceMap(agent)
      val distanceWeight = 2
      val powerWeight = 1
      val rating = (distancePower * distanceWeight + power * powerWeight) / (distanceWeight * powerWeight)
      rating
    }

    var power = 0
    val p = Point(x, y)
    var defendMap = Map.empty[Int, Int]
    var shelterDistanceMap = Map.empty[Int, Int]
    val updatePower = () => power = defendMap.values.map(damageCalc).sum

    lazy val adjs = toTiles(Set((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)))
    lazy val leftDanger = toTiles(Set((x - 1, y), (x - 1, y - 1), (x - 1, y + 1))).map(_.id)
    lazy val rightDanger = toTiles(Set((x + 1, y), (x + 1, y - 1), (x + 1, y + 1))).map(_.id)
    lazy val upDanger = toTiles(Set((x, y - 1), (x + 1, y - 1), (x - 1, y - 1))).map(_.id)
    lazy val downDanger = toTiles(Set((x, y + 1), (x + 1, y + 1), (x - 1, y + 1))).map(_.id)
  }

  case class Agent(id: Int, mine: Boolean, shootCooldown: Int, optimalRange: Int, soakingPower: Int, var splashBombs: Int) extends HasAdjacent {
    var x: Int = _
    var y: Int = _
    var cooldown: Int = _
    var wetness: Int = _

    var enemyAgentsDistanceMap = Map.empty[Int, Int]

    def inShelter(agent: Agent) = ???
    def reachableEnemies = enemyAgentsDistanceMap.filter(kv => kv._2 <= optimalRange/* * 2*/)
    def closestEnemy = reachableEnemies.minByOption(_._2)
    def p = Point(x, y)
    def tile = tiles(y * width + x)
    var target = List.empty[String]
    def print = s"$id; ${target.reverse.mkString("; ")}"

    def adjPoints = toPoints(Set((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1), (x, y),
                                             (x + 1, y + 1), (x - 1, y - 1), (x - 1, y + 1), (x + 1, y - 1)))

    def manhattanAdjs(distance: Int): Set[Point] = {
      val neighbors = for {
        dx <- -distance to distance
        dy = distance - abs(dx)
        sign <- Seq(-1, 1)
        (nx, ny) <- Seq((x + dx, y + dy * sign))
        if nx >= 0 && ny >= 0 && nx < width && ny < height && tiles(ny * width + nx).tileType == 0
      } yield Point(nx, ny)

      neighbors.toSet
    }

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

  def agentTargetMove(agent: Agent) = {
    val enemyAgent = enemyAgents.minBy(enemy => enemy.optimalRange)
    val adjs = enemyAgent.manhattanAdjs(agent.optimalRange)
    adjs
  }

  val updateShelterDistance = () => {
    myAgents.foreach { agent =>
      shelters.foreach { shelter =>
        val distance = shelter.calculateDistance(agent.p)
        shelter.shelterDistanceMap += (agent.id -> distance)
      }
    }
  }

  val updateEnemyDistance = () => {
    myAgents.foreach { agent =>
      enemyAgents.foreach { enemy =>
        val distance = manhattanDistance(enemy.p, agent.p)
        agent.enemyAgentsDistanceMap += (enemy.id -> distance)
      }
    }
  }
  val calculatePoints = () => {
    val enemyPoints = enemyAgents.map(_.p).toSet
    val enemyAdjPoints = enemyAgents.flatMap(enemy => enemy.adjPoints).toSet
    val multiplePoints = enemyAdjPoints.filter(p => p.adjPoints.count(enemyPoints.contains) > 1)
    if (multiplePoints.isEmpty) enemyAdjPoints else multiplePoints
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

  lazy val initialAgentPoints = agents.map(a => (a.id, a.p)).toMap

// -------------------------------------------- game loop --------------------------------------------------------------
  var i = 0

  while(true) {
    val agentCount = readLine.toInt
    if (debug) Console.err.println(s"$agentCount")

    val liveAgents = for(_ <- 0 until agentCount) yield {
      // cooldown: Number of turns before this agent can shoot
      // wetness: Damage (0-100) this agent has taken
      val Array(agentId, x, y, cooldown, splashBombs, wetness) = (readLine split " ").filter(_ != "").map (_.toInt)

      val agent = agentMap(agentId)
      agent.x = x
      agent.y = y
      agent.cooldown = cooldown
      agent.splashBombs = splashBombs
      agent.wetness = wetness
      agent.enemyAgentsDistanceMap = Map.empty

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
    updateShelterDistance()
    updateEnemyDistance()
    val splashPoints = calculatePoints()

    shelters.foreach(_.updatePower())
    var sortedShelters = shelters.sortBy(_.power)(Ordering.Int.reverse)

    myAgents.foreach { agent =>
      val weakestEnemy = enemyAgents.maxBy(_.wetness)
      val pts = weakestEnemy.manhattanAdjs(agent.optimalRange)
      val targetPoint = pts.minByOption(targetPoint => manhattanDistance(agent.p, targetPoint)).getOrElse(weakestEnemy.p)

//      val targetPoint = sortedShelters.headOption.map(t => {
//        sortedShelters = sortedShelters.tail
//        t.p
//      }).getOrElse {
//        val pts = minEnemy.manhattanAdjs(agent.optimalRange)
//        pts.minByOption(targetPoint => manhattanDistance(agent.p, targetPoint)).getOrElse(minEnemy.p)
//      }

      agent.target ::= s"MOVE $targetPoint"

      if (agent.splashBombs > 0) {
        splashPoints.find(splashPoint => manhattanDistance(agent.p, splashPoint) < 5)
//          .orElse(enemyAgents.find(enemy => manhattanDistance(agent.p, enemy.p) < 5).map(_.p))
          .foreach(p => agent.target ::= s"THROW $p")
      }
      if (agent.cooldown == 0 && !agent.target.exists(_.startsWith("T"))) {
        if (manhattanDistance(agent.p, weakestEnemy.p) <= agent.optimalRange && weakestEnemy.tile.defendMap.getOrElse(agent.id, 0) == 0) {
          agent.target ::= s"SHOOT ${weakestEnemy.id}"
        } else {
          agent.closestEnemy.filter(enemyXY => tiles(enemyXY._2 + width + enemyXY._1).defendMap.getOrElse(agent.id, 0) == 0).foreach(shoot => {
            agent.target ::= s"SHOOT ${shoot._1}"
          })
        }
      }

      if (!agent.target.exists(_.startsWith("S")) && !agent.target.exists(_.startsWith("T"))) {
        agent.target ::= s"HUNKER_DOWN; MESSAGE В укрытие!;"
      }

//      agent.target ::= "MESSAGE Только вперёд!"
    }

    myAgents.foreach { agent =>
      println(s"${agent.print}")
      agent.target = List.empty
    }

    shelters.foreach(_.defendMap = Map.empty)
    i += 1
  }
}