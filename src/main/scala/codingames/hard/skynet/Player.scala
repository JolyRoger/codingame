package codingames.hard.skynet

import scala.collection.mutable
import scala.io.Source
import scala.io.StdIn._

object Player extends App {

// ------------------------------------------FILE ENTRY------------------------------------------------------------------
//  val filename = "resources/skynet/ComplexMesh.txt"
//  val bufferedSource = Source.fromFile(filename)
//  val data = bufferedSource.getLines
//  def readInt = if (data.hasNext) data.next.toInt else -1
//  def readLine = if (data.hasNext) data.next else "EOF"
// ----------------------------------------------------------------------------------------------------------------------

  class Graph(N: Int) {
    val adj = (for (i <- 0 until N) yield Set[Int]()).toArray
    var nodes = Map.empty[Int, Set[Int]]

    def addEdge(v: Int, w: Int) {
      adj(v) = adj(v) + w
      adj(w) = adj(w) + v
    }

    def cutEdge(v: Int, w: Int) {
      adj(v) = adj(v) - w
      adj(w) = adj(w) - v
    }

    def bfs(s: Int, marked: Array[Boolean] = new Array[Boolean](n)): (Array[Int], Array[Int]) = {
      val edgeTo = Array.fill[Int](n)(Int.MaxValue)
      val distTo = Array.fill[Int](n)(Int.MaxValue)
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

    def allExitNeighbours(exits: IndexedSeq[Int]): IndexedSeq[Int] = exits.flatMap(adj(_)).sorted

    def getMidNeighbour(node: Int) = adj(node) & midNodes

    def calculateNodes(exits: IndexedSeq[Int]) {
      nodes = Map.empty[Int, Set[Int]]
      exits.foreach(exit => adj(exit).foreach(neighbour =>
        nodes = nodes.get(neighbour) match {
          case None => nodes + (neighbour -> Set(exit))
          case Some(exits) => nodes + (neighbour -> (exits + exit))
        }))
    }

    def dangNodes = nodes.filter(_._2.size > 1).keySet

    def midNodes = nodes.filter(_._2.size == 1).keySet

    def path(from: Int, to: Int, edgeTo: Array[Int], nodeFilter: Int => Boolean = _ => true): List[Int] = {
      if (nodeFilter(from)) {
        if (from == to) List(to) else path(edgeTo(from), to, edgeTo) ::: List(from)
      } else List.empty
    }

  }

  def findClosest(distTo: Array[Int], edgeTo: Array[Int], si: Int) = {
    val exitDist = exits.map(exit => (exit, distTo(exit)))
    val exitDistGroup = exitDist.groupBy(_._2)
    val closestExitDistGroup = exitDistGroup.minBy(_._1)
    val paths = closestExitDistGroup._2.map(closestExitDist => graph.path(closestExitDist._1, si, edgeTo))
    val pathGroup = paths.groupBy(_.tail.head)
    val maxPaths = pathGroup.maxBy(_._1)
    val selectedPath = maxPaths._2.head
    val closestExit0 = selectedPath(selectedPath.length - 1)
    val closestExitNeighbour0 = selectedPath(selectedPath.length - 2)
    (closestExit0, closestExitNeighbour0)
  }

  def findBest(distTo: Array[Int], edgeTo: Array[Int], si: Int) = {
    val dang = graph.dangNodes
    val mid = graph.midNodes
    val marked = Array.fill[Boolean](n)(true)
    mid.foreach(marked(_) = false)
    dang.foreach(marked(_) = false)
    marked(si) = false

    val (_, distMidTo) = graph.bfs(si, marked)

    dang.find(dn => distMidTo(dn) < Int.MaxValue) match {
      case Some(d) => (d, (graph.adj(d) & exits.toSet).head)
      case None => findClosest(distTo, edgeTo, si)
    }
  }

  val Array(n, l, e) = for (i <- readLine split " ") yield i.toInt
  val graph = new Graph(n)
  for (i <- 0 until l) {
    val Array(n1, n2) = for (i <- readLine split " ") yield i.toInt
    graph.addEdge(n1, n2)
  }

  val exits = for (i <- 0 until e) yield readInt

  while (true) {
    val si = readInt
    val (edgeTo, distTo) = graph.bfs(si)
    graph.calculateNodes(exits)
    val distToExit = exits.map(exit => (exit, distTo(exit), edgeTo(exit))).minBy(_._2)
    val cut = if (distToExit._2 == 1) (distToExit._3, distToExit._1)
              else findBest(distTo, edgeTo, si)

    graph.cutEdge(cut._1, cut._2)

    println(s"${cut._1} ${cut._2}")
  }
}
