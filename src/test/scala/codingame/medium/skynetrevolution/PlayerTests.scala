package codingame.medium.skynetrevolution

import codingames.medium.skynetrevolution.Player
import org.scalatest.FlatSpec

class PlayerTests extends FlatSpec {

  "A Player" should "find path" in {
    val exit = 7
    val spider = 1

    val graph = new Player.Graph(10)
//    graph.addEdge(0, 1)
    graph.addEdge(1, 2)
    graph.addEdge(1, 3)
    graph.addEdge(1, 4)
    graph.addEdge(2, 4)
    graph.addEdge(3, 4)
    graph.addEdge(1, 4)
    graph.addEdge(4, 5)
    graph.addEdge(5, 6)
    graph.addEdge(6, 7)
    val (edgeTo, distTo) = graph.bfs(spider)
    val p = graph.path(exit, spider, edgeTo)
    val adjs = graph.sortedPathAdj(p)


//    val mmm = adjs.map(s => s._1 -> s._2).toMap
    val node2 = graph.findNeighbour(adjs.head._1, p)

    Console.err.println(s"${adjs.head._1} $node2")
//    Console.err.println("\nEDGES:")
//    edgeTo.foreach(edge => Console.err.print(s"$edge "))
//    Console.err.println("\nDISTS:")
//    distTo.foreach(dist => Console.err.print(s"$dist "))
  }
}
