package codingame.challenge.xmasrush

import codingames.challenge.xmasrush.v1.Player.{connected, toMatrix, toNumber}
import codingames.challenge.xmasrush.v1.{Graph, Player}
import org.scalatest.FlatSpec

class PlayerTests extends FlatSpec {
  val items = List((3, 2), (0, 6), (0, 4), (1, 2), (5, 5))
  def print(inputs: IndexedSeq[Array[String]]) = {

    inputs.foreach(input => {
      Console.err.println
      input.foreach(tile => Console.err.print(" " + tile))
    })
    Console.err.println
  }

  val inputs = IndexedSeq(
    Array("0110", "0101", "1011", "1010", "0110", "1010", "1101"),
    Array("0110", "1101", "1011", "0110", "1010", "1011", "1101"),
    Array("1111", "0111", "1010", "0111", "0011", "1001", "1101"),
    Array("1001", "1001", "1010", "1010", "1010", "0110", "0110"),
    Array("0111", "0110", "1100", "1101", "1010", "1101", "1111"),
    Array("0111", "1110", "1010", "1011", "1110", "0111", "1001"),    // FIXME (3)
    Array("0111", "1010", "1001", "1010", "1110", "0101", "1001")
  )

  val inputs_ = IndexedSeq(
  Array("0110", "0110", "1111", "1101", "0011", "1001", "1101"),
  Array("1011", "1010", "1011", "0111", "0110", "0101", "1101"),
  Array("0011", "0101", "0111", "1010", "0111", "0110", "0101"),
  Array("0110", "1101", "0101", "0101", "0101", "0111", "1001"),
  Array("1001", "0101", "1001", "1101", "1010", "1101", "0101"),
  Array("0111", "0101", "1001", "1101", "1110", "1010", "1110"),
  Array("0111", "0101", "0111", "1011", "0111", "1111", "1001")
//  Array("1001", "0111", "0110", "1100", "0111", "1111", "1001")
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
    assert(number === 17)
  }

  "A Player" should "check if two points are connected" in {
    val p1 = (6,4)
    val p2 = (6,3)
    val result = Player.connected(p1, p2, inputs)
    println(s"$p1 and $p2 connection result is $result")
    assert(result)

    val p3 = (3,5)
    val p4 = (4,5)
    val result2 = Player.connected(p3, p4, inputs)
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
    val graph = Player.createGraph(inputs)
    val (edges, dists) = graph.bfs((0, 0))
    val reachableItems = items.filter(item => edges(item) != Int.MaxValue)
    val path = Player.path((0, 0), reachableItems.head, edges)

    println(s"edges(3,2): ${edges((3,2))}")
    println(s"Path (0,0)->(3,2): $path")
    assert(edges((3,2)) != Int.MaxValue)
    assert(path == "RIGHT RIGHT DOWN DOWN DOWN DOWN RIGHT UP UP")
  }

  "A Player" should "find closest point in path" in {
    val player = (2,6)
    val myItem = (2,0)

    val graph = Player.createGraph(inputs)

    val edges = graph.dfs(player)
    val vert = (for (i <- edges.indices
                     if edges(i) != Int.MaxValue) yield Array(i, edges(i))).flatten.distinct.filter(_ != toNumber(player)).map(toMatrix).toList

    val cl = Player.closest(myItem, vert)
    println(s"closest: $cl")
    println(s"vert: $vert")
    val path = Player.path(player, cl, edges)
    println(s"path: $path")
    assert(cl == (4,2))
    assert(path == "RIGHT RIGHT DOWN DOWN DOWN DOWN RIGHT UP UP RIGHT")
  }

  "A Player" should "collect all reachable items" in {
    val player = (0,0)
    val myItem1 = toNumber((0,1))
    val myItem2 = toNumber((2,3))
    val myItem3 = toNumber((4,2))
    val reachableItems = List(myItem1, myItem2, myItem3)
    val graph = Player.createGraph(inputs)
    val edges = graph.dfs(player)
    val path = Player.collectAll(player, reachableItems, graph, edges)
    val limitedPath = path.split("\\s").take(10).reduce((a1,a2) => a1 + " " + a2)
    val extraLimitedPath = path.split("\\s").take(20).reduce((a1,a2) => a1 + " " + a2)
    println(s"pp: $path")
    println(s"lp: $limitedPath")
    println(s"ep: $extraLimitedPath")
  }

  "A Player" should "find best push" in {
    val player = (6,3)
    val myItem = (3,5)
    val myTile = "0101"
    print(inputs)
//    val push = Player.findBestPush(inputs, List(myItem), player, myTile)
//    println(s"best push: $push")
//    Console.err.println
//    print(push)
//    assert(push == ("RIGHT",5))
  }
}
