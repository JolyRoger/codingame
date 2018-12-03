import scala.collection.mutable
/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/
object Solution extends App {
  class Graph(N: Int) {
    val adj = (for (i <- 0 until N) yield List[Int]()).toArray
    def n = N
    def addEdge(v: Int, w: Int){
      adj(v) = w :: adj(v)
      adj(w) = v :: adj(w)
    }
  }

  def bfs(g: Graph, s: Int) = {
    val marked: Array[Boolean] = new Array[Boolean](g.n)
    val distTo = Array.fill[Int](g.n)(Int.MaxValue)
    val q = mutable.Queue[Int]()

    q.enqueue(s)
    marked(s) = true
    distTo(s) = 0
    while (q.nonEmpty) {
      val v = q.dequeue
      g.adj(v).filterNot(marked).foreach(
        w => {
          q.enqueue(w)
          marked(w) = true
          distTo(w) = distTo(v) + 1
        }
      )
    }
    distTo.filter(_ < Int.MaxValue)
  }

  val n = readInt // the number of adjacency relations
  val links = (0 until n).map(_ => for(j <- readLine split " ") yield j.toInt)
  val flattenLinks = links.flatten
  val vertices = flattenLinks.distinct
  val v = flattenLinks.max
  val graph = new Graph(v+1)
  links.foreach(link => graph.addEdge(link(0), link(1)))
  val res = vertices.map(bfs(graph, _)).map(a =>
    a.foldLeft(a(0)) { case (mx, e) => math.max(mx, e) }
  ).min
  println(res)
}