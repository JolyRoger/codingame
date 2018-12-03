package codingames.medium.skynetrevolution

import scala.collection.mutable

/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/


object Player extends App {
  // N — nodes amount, L — links amount
  class Graph(N: Int, L: Int) {
    val adj = (for (i <- 0 until N) yield List[Int]()).toArray
    def n = N
    def addEdge(v: Int, w: Int){
      adj(v) = w :: adj(v)
      adj(w) = v :: adj(w)
    }
    def cutEdge(v: Int, w: Int): Unit = {
      adj(v) = adj(v).filter(_ != w)
      adj(w) = adj(w).filter(_ != v)
    }
  }

  def bfs(g: Graph, s: Int) = {
    val marked: Array[Boolean] = new Array[Boolean](g.n)
    val edgeTo = new Array[Int](g.n)
    val distTo = Array.fill[Int](g.n)(Int.MaxValue)
    val q = mutable.Queue[Int]()
    var i = 0

    q.enqueue(s)
    marked(s) = true
    distTo(s) = 0
    while (q.nonEmpty) {
      val v = q.dequeue
      i = i + 1
      g.adj(v).filterNot(marked).foreach(
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
  // n: the total number of nodes in the level, including the gateways
  // l: the number of links
  // e: the number of exit gateways
  val Array(n, l, e) = for(i <- readLine split " ") yield i.toInt
  val graph = new Graph(n, l)
  for(i <- 0 until l) {
    // n1: N1 and N2 defines a link between these nodes
    val Array(n1, n2) = for(i <- readLine split " ") yield i.toInt
    graph.addEdge(n1, n2)
  }

  val exits = for(i <- 0 until e) yield readInt

  // game loop
  while(true) {
    val si = readInt // The index of the node on which the Skynet agent is positioned this turn
    val res = bfs(graph, si)

    val closestExit = exits.reduce((e1,e2) => {
      if (res._2(e1) < res._2(e2)) e1 else e2
    })

    // Write an action using println
    // To debug: Console.err.println("Debug messages...")

    graph.cutEdge(closestExit, res._1(closestExit))
    // Example: 0 1 are the indices of the nodes you wish to sever the link between
    println(closestExit + " " + res._1(closestExit))
  }
}
