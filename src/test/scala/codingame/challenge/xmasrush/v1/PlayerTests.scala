package codingame.challenge.xmasrush

import codingames.challenge.xmasrush.v1.Player.toNumber
import codingames.challenge.xmasrush.v1.Player.connected
import codingames.challenge.xmasrush.v1.{Graph, Player}
import org.scalatest.FlatSpec

class PlayerTests extends FlatSpec {
    val items = List((3,2),(0,6),(0,4),(1,2),(5,5))
    val inputs = IndexedSeq(
      Array("0110", "0101", "1011", "1010", "0110", "1010", "1101"),
      Array("0110", "1101", "1011", "0110", "1010", "1011", "1101"),
      Array("1111", "0111", "1010", "0111", "0011", "1001", "1101"),
      Array("1001", "1001", "1010", "1010", "1010", "0110", "0110"),
      Array("0111", "0110", "1100", "1101", "1010", "1101", "1111"),
      Array("0111", "1110", "1010", "1001", "1110", "0111", "1001"),
      Array("0111", "1010", "1001", "1010", "1110", "0101", "1001")
    )

  "A Player" should "return closest point" in {
    val playerPoint = (5, 1)
    val distance = List((2, 2), (3, 4), (5, 0))
    val distance1 = Player.euclidean(playerPoint, distance(0))
    val distance2 = Player.euclidean(playerPoint, distance(1))
    val distance3 = Player.euclidean(playerPoint, distance(2))
    println(s"distance1=$distance1\tdistance2=$distance2\tdistance3=$distance3")
    val closestPoint = Player.closest(playerPoint, distance)
    println(s"closest=$closestPoint")
    assert(closestPoint === (5, 0))
  }

  "A Player" should "convert point to number and vice versa" in {
    val point = Player.toMatrix(9)
    val number = Player.toNumber((2,3))
    println(s"(2,3) -> number=$number\t9 point = $point")
    assert(point === (1, 2))
    assert(number === 23)
  }

  "A Player" should "check if two points are connected" in {
    val p1 = (6,4)
    val p2 = (6,3)
    val result = Player.connected(Player.toNumber(p1), Player.toNumber(p2), inputs)
    println(s"$p1 and $p2 connection result is $result")
    assert(result)

    val p3 = (3,5)
    val p4 = (4,5)
    val result2 = Player.connected(Player.toNumber(p3), Player.toNumber(p4), inputs)
    println(s"$p3 and $p4 connection result is $result2")
    assert(!result2)
  }

  "A Player" should "find relations between squares" in {
    val rel1 = Player.relation((2,2), (2,3))
    println(s"(2,2) is $rel1 to (2,3)")
    assert(rel1 == "UP")

    val rel2 = Player.relation((5,5), (5,4))
    println(s"(5,5) is $rel2 to (5,4)")
    assert(rel2 == "DOWN")

    val rel3 = Player.relation((0,6), (1,6))
    println(s"(0,6) is $rel3 to (1,6)")
    assert(rel3 == "LEFT")

    val rel4 = Player.relation((5,1), (4,1))
    println(s"(5,1) is $rel4 to (4,1)")
    assert(rel4 == "RIGHT")

    val rel5 = Player.relation((2,0), (6,2))
    println(s"(2,0) is $rel5 to (6,2)")
    assert(rel5 == "NOTHING")
  }

  "A Player" should "find path in graph" in {
    val graph = new Graph(49)
    for (i <- 0 until 6) {
      for (j <- 0 until 6) {
        if (connected(toNumber((i, j)), toNumber((i, j + 1)), inputs)) graph.addEdge(toNumber((i, j)), toNumber((i, j + 1)))
        if (connected(toNumber((i, j)), toNumber((i + 1, j)), inputs)) graph.addEdge(toNumber((i, j)), toNumber((i + 1, j)))
      }
    }
    val (edges, dists) = graph.bfs(toNumber(0, 0))
    val reachableItems = items.filter(item => edges(toNumber(item)) != Int.MaxValue)
    val path = Player.path(toNumber(0, 0), toNumber(reachableItems.head), edges)

    println(s"edges(3,2): ${edges(toNumber(3,2))}")
    println(s"Path (0,0)->(3,2): $path")
    assert(edges(toNumber(3,2)) != Int.MaxValue)
    assert(path == "RIGHT RIGHT DOWN DOWN DOWN DOWN RIGHT UP UP")
  }
}
