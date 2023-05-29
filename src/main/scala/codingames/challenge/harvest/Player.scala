package codingames.challenge.harvest

import math._
import scala.collection.mutable
import scala.io.Source
import scala.util._
import scala.io.StdIn._

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/

case class Path(from: Int, to: Int, dist: Int)
case class Base(id: Int, bfsResult: (Array[Int], Array[Int])) {
  val edgeTo = bfsResult._1
  val distTo = bfsResult._2
}
class Hexagon(val id: Int, val sort: Int, var resources: Int, var myAnts: Int, var oppAnts: Int) {

}

class Graph(N: Int) {
  val adj = (for (i <- 0 until N) yield Set.empty[Int]).toArray
  def addEdge(v: Int, w: Int){
    adj(v) = adj(v) + w
    adj(w) = adj(w) + v
  }
  def cutEdge(v: Int, w: Int): Unit = {
    adj(v) = adj(v).filter(_ != w)
    adj(w) = adj(w).filter(_ != v)
  }

  def dfs(v: Int): Array[Int] = {
    val marked: Array[Boolean] = new Array[Boolean](N)
    val edgeTo = Array.fill[Int](N)(Int.MaxValue)
    def internalDfs(v: Int, edges: Array[Int]): Array[Int] = {
      marked(v) = true
      for (w <- adj(v)) {
        if (!marked(w)) {
          internalDfs(w, edges)
          edges(w) = v
        }
      }
      edges
    }
    internalDfs(v, edgeTo)
  }

  def bfs(s: Int) = {
    val marked: Array[Boolean] = new Array[Boolean](N)
    val edgeTo = Array.fill[Int](N)(Int.MaxValue)
    val distTo = Array.fill[Int](N)(Int.MaxValue)
    val q = mutable.Queue[Int]()
    var i = 0

    q.enqueue(s)
    marked(s) = true
    distTo(s) = 0
    while (q.nonEmpty) {
      val v = q.dequeue
      i = i + 1
      adj(v).filterNot(marked).foreach(
        w => {
          q.enqueue(w)
          marked(w) = true
          edgeTo(w) = v
          distTo(w) = distTo(v) + 1
        }
      )
    }
    (edgeTo, distTo)
  }

  def getGroup(s: Int, cond: Int => Boolean) = {
    var gr = Set(s)
    var stack = List(s)
    var marked = Set[Int](s)

    while (stack.nonEmpty) {
      val elem = stack.head
      stack = stack.tail
      val neighbours = adj(elem).filter(neighbour => cond(neighbour) && !marked.contains(neighbour)).toList
      neighbours.foreach(n => marked = marked + n)
      stack = neighbours ::: stack
      gr = gr ++ neighbours.toSet
    }
    gr
  }

}

object Player extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
   val filename = "resources/harvest/1.txt"
   val bufferedSource = Source.fromFile(filename)
   val data = bufferedSource.getLines
   def readInt = if (data.hasNext) data.next.toInt else {System.exit(0); -1}
   def readLine = if (data.hasNext) data.next else {System.exit(0); ""}
//----------------------------------------------------------------------------------------------------------------------
  val n = 100
  val numberOfCells = readLine.toInt // amount of hexagonal cells in this map
  // Console.err.println(s"$numberOfCells")

  val graph = new Graph(numberOfCells)
  val hexagons = Array.ofDim[Hexagon](numberOfCells)
  var resourceMap = Map.empty[Int, Hexagon]
  var eggMap = Map.empty[Int, Hexagon]
  var myAntSet = Set.empty[Hexagon]

  for(i <- 0 until numberOfCells) {
    // _type: 0 for empty, 1 for eggs, 2 for crystal
    // initialResources: the initial amount of eggs/crystals on this cell
    // neigh0: the index of the neighbouring cell for each direction
    val Array(_type, initialResources, neigh0, neigh1, neigh2, neigh3, neigh4, neigh5) = (readLine split " ").filter(_ != "").map (_.toInt)
    hexagons(i) = new Hexagon(i, _type, initialResources, 0, 0)

    if (neigh0 > -1) graph.addEdge(i, neigh0)
    if (neigh1 > -1) graph.addEdge(i, neigh1)
    if (neigh2 > -1) graph.addEdge(i, neigh2)
    if (neigh3 > -1) graph.addEdge(i, neigh3)
    if (neigh4 > -1) graph.addEdge(i, neigh4)
    if (neigh5 > -1) graph.addEdge(i, neigh5)
    // Console.err.println(s"${_type}, $initialResources, $neigh0, $neigh1, $neigh2, $neigh3, $neigh4, $neigh5")
  }
  val numberOfBases = readLine.toInt
  // Console.err.println(s"$numberOfBases")
  var inps = readLine
  // Console.err.println(s"$inps")
  var inputs = inps split "\\s"
  val myBaseIndices = for(i <- 0 until numberOfBases) yield inputs(i).toInt

  inps = readLine
  // Console.err.println(s"$inps")
  inputs = inps split "\\s"
  val oppBaseIndices = for(i <- 0 until numberOfBases) yield inputs(i).toInt

  def groupFilter(resourceIndex: Int) = resourceMap.contains(resourceIndex)




  for (_ <- LazyList.from(0).takeWhile(_ < n)) {
    resourceMap = Map.empty
    myAntSet = Set.empty

    for(i <- 0 until numberOfCells) {
      // resources: the current amount of eggs/crystals on this cell
      // myAnts: the amount of your ants on this cell
      // oppAnts: the amount of opponent ants on this cell
      val Array(resources, myAnts, oppAnts) = (readLine split "\\s").withFilter(_.nonEmpty).map(_.toInt)
      hexagons(i).resources = resources
      hexagons(i).myAnts = myAnts
      hexagons(i).oppAnts = oppAnts

      if (resources > 0) {
        if (hexagons(i).sort == 1) {
          eggMap = eggMap + (i -> hexagons(i))
        } else if (hexagons(i).sort == 2) {
          resourceMap = resourceMap + (i -> hexagons(i))
        }
      }
      if (myAnts > 0) myAntSet = myAntSet + hexagons(i)

      // Console.err.println(s"$resources, $myAnts, $oppAnts")
    }

    var groupKeys = Set.empty[Int]

    val grs = resourceMap.keys.collect {
      case resourceKey if (!groupKeys.contains(resourceKey)) => {
        val newGroup = graph.getGroup(resourceKey, groupFilter)
        groupKeys = groupKeys ++ newGroup
        newGroup
      }
    }
//    val grs = graph.getGroup(resourceMap.keys.head.toInt, groupFilter)

    val bases = myBaseIndices.map(index => Base(index, graph.bfs(index)))
    val oppBases = oppBaseIndices.map(index => Base(index, graph.bfs(index)))

    val resourceVals = resourceMap.values
    val resDist = resourceVals.flatMap(resourceHex => bases.map(base => Path(base.id, resourceHex.id, base.distTo(resourceHex.id))))
    val oppResDist = resourceVals.flatMap(resourceHex => oppBases.map(base => Path(base.id, resourceHex.id, base.distTo(resourceHex.id))))
//    val dists = resDist.groupBy(_.dist)
    val oppDists = oppResDist.groupBy(_.to)

//    val shortestRes = dists(dists.minBy(_._1)._1).maxBy(path => resourceSet(path.to).resources)
    val strategy = 0

    val commands = if (strategy == 0) {
      val filteredResDists = resDist.filter(p => p.dist <= oppDists(p.to).minBy(_.dist).dist)
      val targetResDists = if (filteredResDists.isEmpty) resDist else filteredResDists
      targetResDists.map(p => s"LINE ${p.from} ${p.to} ${100 / p.dist}").mkString(";")
    } else if (strategy == 1) { // mine closer
      val filteredResDists = resDist.groupBy(p => p.dist.compareTo(oppDists(p.to).minBy(_.dist).dist))
      filteredResDists.get(0).orElse(filteredResDists.get(-1)).orElse(filteredResDists.get(1))
        .map(targetResDists => targetResDists.map(p => s"LINE ${p.from} ${p.to} ${100 / p.dist}").mkString(";")).getOrElse("WAIT")
    } else if (strategy == 2) { // opp closer
      val filteredResDists = resDist.groupBy(p => p.dist.compareTo(oppDists(p.to).minBy(_.dist).dist))
      filteredResDists.get(0).orElse(filteredResDists.get(1)).orElse(filteredResDists.get(-1))
        .map(targetResDists => targetResDists.map(p => s"LINE ${p.from} ${p.to} ${100 / p.dist}").mkString(";")).getOrElse("WAIT")
    } else "WAIT"
//    val shortestRes = resDist.minBy(_.dist)
    // Write an action using println
    // To debug: // Console.err.println("Debug messages...")

    // WAIT | LINE <sourceIdx> <targetIdx> <strength> | BEACON <cellIdx> <strength> | MESSAGE <text>
    println(commands)
  }
}