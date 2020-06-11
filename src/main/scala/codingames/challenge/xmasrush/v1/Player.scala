package codingames.challenge.xmasrush.v1

import math._
import scala.collection.mutable
import scala.io.StdIn._

class Graph(N: Int) {
  val adj = (for (i <- 0 until N) yield List[Int]()).toArray
  def addEdge(v: Int, w: Int){
    adj(v) = w :: adj(v)
    adj(w) = v :: adj(w)
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
}














/**
  * Help the Christmas elves fetch presents in a magical labyrinth!
  **/
object Player extends App {
  def createGraph(inputs: IndexedSeq[Array[String]])= {
    val graph = new Graph(49)
    for (i <- 0 until 6) {
      for (j <- 0 until 6) {
        if (connected(toNumber((i, j)), toNumber((i, j + 1)), inputs)) graph.addEdge(toNumber((i, j)), toNumber((i, j + 1)))
        if (connected(toNumber((i, j)), toNumber((i + 1, j)), inputs)) graph.addEdge(toNumber((i, j)), toNumber((i + 1, j)))
      }
    }
    for (k <- 0 until 6) {
      if (connected((6, k), (6, k + 1), inputs)) graph.addEdge((6, k), (6, k + 1))
      if (connected((k, 6), (k + 1, 6), inputs)) graph.addEdge((k, 6), (k + 1, 6))
    }
    graph
  }
  def closest(p: (Int, Int), items: List[(Int, Int)]) = if (items.isEmpty) (3,3) else items.minBy(item => euclidean(p, item))
  def euclidean(a: (Int, Int), b: (Int, Int)) = sqrt(pow(b._1 - a._1, 2) + pow(b._2 - a._2, 2))
  def possibleDirections(plSquare: (Int, Int), player: String, neighbours: Array[String]) = {
    val Array(pUp, pRight, pDown, pLeft) = player split ""
    val upperDown = if (neighbours.head == null) "0" else neighbours.head.split("")(2)
    val rightLeft = if (neighbours(1) == null) "0" else neighbours(1).split("")(3)
    val downUp = if (neighbours(2) == null) "0" else neighbours(2).split("")(0)
    val leftRight = if (neighbours(3) == null) "0" else neighbours(3).split("")(1)

    Map(
      (plSquare._1, plSquare._2 - 1) -> (if (pUp == "1" && pUp == upperDown) "UP" else null),
      (plSquare._1, plSquare._2 + 1) -> (if (pDown == "1" && pDown == downUp) "DOWN" else null),
      (plSquare._1 - 1, plSquare._2) -> (if (pLeft == "1" && pLeft == leftRight) "LEFT" else null),
      (plSquare._1 + 1, plSquare._2) -> (if (pRight == "1" && pRight == rightLeft) "RIGHT" else null)
    ).filter(_._2 != null)
  }
  def toMatrix(number: Int): (Int, Int) = (number / 7, number % 7)
  implicit def toNumber(point: (Int, Int)): Int = point._1 * 7 + point._2 % 7
  // p1 is directly <result: {UP, DOWN, LEFT, RIGHT, NOTHING}> to p2
  def relation(p1: (Int, Int), p2: (Int, Int)) = if (p1._1 == p2._1 && p1._2 - 1 == p2._2) "DOWN" else
    if (p1._1 == p2._1 && p1._2 + 1 == p2._2) "UP" else
    if (p1._2 == p2._2 && p1._1 - 1 == p2._1) "RIGHT" else
    if (p1._2 == p2._2 && p1._1 + 1 == p2._1) "LEFT" else "NOTHING"
  def connected(p1: Int, p2: Int, inputs: IndexedSeq[Array[String]]) = {
    val pp1 = toMatrix(p1)
    val pp2 = toMatrix(p2)
    val p1Inputs = inputs(pp1._2)(pp1._1)
    val p2Inputs = inputs(pp2._2)(pp2._1)
    val p1p2Relation = relation(pp1, pp2)
    if (p1p2Relation == "NOTHING") false else
    if (p1p2Relation == "UP") p1Inputs.split("")(2).toInt == 1 && p2Inputs.split("")(0).toInt == 1 else
    if (p1p2Relation == "DOWN") p1Inputs.split("")(0).toInt == 1 && p2Inputs.split("")(2).toInt == 1 else
    if (p1p2Relation == "LEFT") p1Inputs.split("")(1).toInt == 1 && p2Inputs.split("")(3).toInt == 1 else
    if (p1p2Relation == "RIGHT") p1Inputs.split("")(3).toInt == 1 && p2Inputs.split("")(1).toInt == 1 else false
  }
  def path(from: Int, to: Int, edges: Array[Int]): String = {
    val mfrom = toMatrix(from)
    val mTo = toMatrix(to)
    val next = edges(to)
    if (next == from) relation(mTo, toMatrix(from)) else
      path(from, next, edges) + " " + relation(mTo, toMatrix(next))
  }
  def collectAll(from: Int, reachableItems: List[Int], graph: Graph, edges: Array[Int]): String = {
    if (reachableItems.tail.isEmpty) path(from, reachableItems.head, edges) else {
      path(from, reachableItems.head, edges) + " " + collectAll(reachableItems.head, reachableItems.tail, graph, graph.dfs(reachableItems.head))
    }
  }
  def findBestPush(inputs: IndexedSeq[Array[String]], myItems: List[(Int, Int)], from: (Int, Int), playertile: String, myItem: (Int, Int)): (String, String) = {
    // RIGHT
    for (hor <- 0 until 7) {
      var _inputs = for(inner <- inputs) yield {for (elem <- inner) yield elem}
      for(i <- (1 to 6).reverse) _inputs(hor)(i) = inputs(hor)(i-1)
      _inputs(hor)(0) = playertile
      val _player = if (from._2 == hor) if (from._1 < 6) (from._1 + 1, hor) else (0, hor) else from
      val movedItems = myItems.map(myItem => if (myItem._2 == hor) if (myItem._1 < 6) (myItem._1 + 1, hor) else (-2, -2) else myItem).filter(myItem => myItem._1 >= 0)
      val edges = createGraph(_inputs).dfs(_player)
      val reachableItems = movedItems.filter(myItem => edges((myItem._1, myItem._2)) != Int.MaxValue)
      if (reachableItems.nonEmpty) {
        return ("RIGHT", hor + " ")
      }
    }
    // LEFT
    for (hor <- 0 until 7) {
      var _inputs = for(inner <- inputs) yield {for (elem <- inner) yield elem}
      for(i <- 0 until 6) _inputs(hor)(i) = inputs(hor)(i+1)
      _inputs(hor)(6) = playertile
      val _player = if (from._2 == hor) if (from._1 > 0) (from._1 - 1, hor) else (6, hor) else from
      val movedItems = myItems.map(myItem => if (myItem._2 == hor) if (myItem._1 > 0) (myItem._1 - 1, hor) else (-2, -2) else myItem).filter(myItem => myItem._1 >= 0)
      val edges = createGraph(_inputs).dfs(_player)
      val reachableItems = movedItems.filter(myItem => edges((myItem._1, myItem._2)) != Int.MaxValue)
      if (reachableItems.nonEmpty) {
        return ("LEFT", hor + " ")
      }
    }
    // UP
    for (ver <- 0 until 7) {
      var _inputs = for(inner <- inputs) yield {for (elem <- inner) yield elem}
      for(i <- 0 until 6) _inputs(i)(ver) = inputs(i+1)(ver)
      _inputs(6)(ver) = playertile
      val _player = if (from._1 == ver) if (from._2 > 0) (ver, from._2 - 1) else (ver, 6) else from
      val movedItems = myItems.map(myItem => if (myItem._1 == ver) if (myItem._2 > 0) (ver, myItem._2 - 1) else (-2, -2) else myItem).filter(myItem => myItem._1 >= 0)
      val edges = createGraph(_inputs).dfs(_player)
      val reachableItems = movedItems.filter(myItem => edges((myItem._1, myItem._2)) != Int.MaxValue)
      if (reachableItems.nonEmpty) {
        return ("UP", ver + " ")
      }
    }
    // DOWN
    for (ver <- 0 until 7) {
      var _inputs = for(inner <- inputs) yield {for (elem <- inner) yield elem}
      for(i <- (1 to 6).reverse) _inputs(ver)(i) = inputs(i-1)(ver)
      _inputs(6)(ver) = playertile
      val _player = if (from._1 == ver) if (from._2 < 6) (ver, from._2 + 1) else (ver, 0) else from
      val movedItems = myItems.map(myItem => if (myItem._1 == ver) if (myItem._2 < 6) (ver, myItem._2 + 1) else (-2, -2) else myItem).filter(myItem => myItem._1 >= 0)
      val edges = createGraph(_inputs).dfs(_player)
      val reachableItems = movedItems.filter(myItem => edges((myItem._1, myItem._2)) != Int.MaxValue)
      if (reachableItems.nonEmpty) {
        return ("DOWN", ver + " ")
      }
    }

    val direction = if (from._1 > myItem._1 && from._2 != myItem._2) "LEFT" else
      if (from._1 < myItem._1 && from._2 != myItem._2) "RIGHT" else
      if (from._2 < myItem._2 && from._1 != myItem._1) "DOWN" else
      if (from._2 > myItem._2 && from._1 != myItem._1) "UP" else "DOWN"
    val rowid = if (direction == "LEFT" || direction == "RIGHT") from._2 else from._1
    (direction, rowid + " ")
  }

  var i = 0

  // game loop
  while (true) {
    val turntype = readInt
    val inputs = for (i <- 0 until 7) yield readLine split " "
    var players: List[(Int, Int)] = Nil
    var myTile: String = ""

    for (i <- 0 until 2) {
      // numplayercards: the total number of quests for a player (hidden and revealed)
      val Array(_numplayercards, _playerx, _playery, playertile) = readLine split " "
      val numplayercards = _numplayercards.toInt
      val playerx = _playerx.toInt
      val playery = _playery.toInt
      val myTile = if (i == 0) playertile
      players = (playerx, playery) :: players
    }
    val numitems = readInt // the total number of items available on board and on player tiles

    var items: List[(Int, Int, String)] = Nil

    for (i <- 0 until numitems) {
      val Array(itemname, _itemx, _itemy, _itemplayerid) = readLine split " "
      val itemx = _itemx.toInt
      val itemy = _itemy.toInt
      val itemplayerid = _itemplayerid.toInt
      if (itemplayerid == 0) items = ((itemx, itemy, itemname) :: items).filter(item => item._1 >=0 && item._2 >= 0)
    }

    val numquests = readInt // the total number of revealed quests for both players
    val quests = (for (i <- 0 until numquests) yield readLine split " ").filter(_(1).toInt == 0)
    val myItems = items.filter(item => quests.map(_(0)).contains(item._3)).map(item => (item._1, item._2))
    val myItem = if (myItems.isEmpty) closest(players(1), items.map(item => (item._1, item._2))) else myItems.head

    val pb = possibleDirections((players(1)._1, players(1)._2), inputs(players(1)._2)(players(1)._1),
      Array(
        if (players(1)._2 - 1 >= 0) inputs(players(1)._2 - 1)(players(1)._1) else null,
        if (players(1)._1 + 1 <= 6) inputs(players(1)._2)(players(1)._1 + 1) else null,
        if (players(1)._2 + 1 <= 6) inputs(players(1)._2 + 1)(players(1)._1) else null,
        if (players(1)._1 - 1 >= 0) inputs(players(1)._2)(players(1)._1 - 1) else null
      )
    )

    val movetype = if (i % 2 == 0) "PUSH" else if (pb.isEmpty) "PASS" else "MOVE"

    var (direction, rowid) = if (movetype == "PUSH") {
        findBestPush(inputs, myItems, players(1), myTile, myItem)
    } else if (movetype == "MOVE") {
        val graph = createGraph(inputs)
        val edges = graph.dfs(players(1))
        val reachableItems = myItems.filter(myItem => edges((myItem._1, myItem._2)) != Int.MaxValue)
        if (reachableItems.nonEmpty) {
          (collectAll(players(1), reachableItems.map(toNumber), graph, edges).split("\\s").take(20).reduce((a1,a2) => a1 + " " + a2), "")
        }  else {
          val vert = (for (i <- edges.indices
                           if edges(i) != Int.MaxValue) yield Array(i, edges(i))).flatten.distinct.filter(_ != toNumber(players(1))).map(toMatrix).toList

          (path(players(1), closest((myItem._1, myItem._2), vert), edges), "")
        }
      } else if (movetype == "PASS") ("", "")

//    val rowid = if (movetype == "PUSH")
//        (if (direction == "LEFT" || direction == "RIGHT") players(1)._2 else players(1)._1) + " "
    else ""
      println(s"$movetype $rowid$direction".trim) // PUSH <id> <direction> | MOVE <direction> | PASS
    i += 1
  }
}
