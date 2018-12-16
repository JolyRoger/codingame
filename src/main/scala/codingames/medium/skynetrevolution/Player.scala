//package codingames.medium.skynetrevolution

import scala.annotation.tailrec
import scala.collection.mutable

object Player extends App {

  class Graph(N: Int) {
    val adj = (for (i <- 0 until N) yield List[Int]()).toArray

    def n = N

    def addEdge(v: Int, w: Int) {
      adj(v) = w :: adj(v)
      adj(w) = v :: adj(w)
    }

    def cutEdge(v: Int, w: Int): Unit = {
      adj(v) = adj(v).filter(_ != w)
      adj(w) = adj(w).filter(_ != v)
    }

    def bfs(s: Int) = {
      val marked: Array[Boolean] = new Array[Boolean](n)
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

    def path(from: Int, to: Int, edgeTo: Array[Int]): List[Int] = if (from == to) List(to) else List(from) ::: path(edgeTo(from), to, edgeTo)

    def sortedPathAdj(path: List[Int]) =
      (for (i <- adj.indices if path.contains(i)) yield i -> adj(i)).sortWith(_._2.size < _._2.size).toMap

    def findNeighbour(node: Int, path: List[Int]): Int = {
      for (i <- path.indices) {
        if (path(i) == node) {
          return if (i > 0) path(i - 1) else path(i + 1)
        }
      }
      -1
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
    val closestExit = exits.minBy(distTo)
    val p = graph.path(closestExit, si, edgeTo)
    val adjs = graph.sortedPathAdj(p)
    val node2 = graph.findNeighbour(adjs.head._1, p)

    graph.cutEdge(adjs.head._1, node2)
    println(adjs.head._1 + " " + node2)
  }
}
