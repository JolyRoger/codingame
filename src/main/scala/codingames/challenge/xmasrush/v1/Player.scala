package codingames.challenge.xmasrush.v1

import math._
import scala.collection.mutable


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

  def closest(p: (Int, Int), items: List[(Int, Int)]) = if (items.isEmpty) (-1,-1) else items.minBy(item => euclidean(p, item))
  def euclidean(a: (Int, Int), b: (Int, Int)) = sqrt(pow(b._1 - a._1, 2) + pow(b._2 - a._2, 2))
  def possibleDirections(plSquare: (Int, Int), player: String, neighbours: Array[String]) = {
    val Array(pUp, pRight, pDown, pLeft) = player split ""
//    Console.err.println("Player: " + pUp + " " + pRight + " " + pDown+ " " + pLeft)
//    Console.err.println("Up: " + neighbours(0))
//    Console.err.println("Right: " + neighbours(1))
//    Console.err.println("Down: " + neighbours(2))
//    Console.err.println("Left: " + neighbours(3))
//    Console.err.println(s"plSquare: $plSquare")
//    neighbours.foreach(a => Console.err.print(a + ":: "))
//    Console.err.println()
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
  def toNumber(point: (Int, Int)): Int = point._1 * 7 + point._2 % 7
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
//    Console.err.println(s"from=$from\tto=$to\tedges size=${edges.size}")
//    for (i <- 0 until 49) {
//      Console.err.print(i + ":" + edges(i)+"\t")
//    }
    val next = edges(to)
    if (next == from) relation(mTo, toMatrix(from)) else
      path(from, next, edges) + " " + relation(mTo, toMatrix(next))
  }

  var i = 0

  // game loop
  while (true) {
    val turntype = readInt
//    Console.err.println("turntype: " + turntype)
//    Console.err.println("++++++++++++++++++++++++++++")
//    var tiles: Array[Array[Int]] =
    var inputs = for (i <- 0 until 7) yield readLine split " "
    connected(1,2,inputs)
//      for (i <- 0 until 7) {
//      var inputs = readLine split " "
//      inputs.flatten.foreach(Console.err.println)
//      for (j <- 0 until 7) {
//
//      }
//                  Console.err.println("------------------------")
//    }
//    Console.err.println("++++++++++++++++++++++++++++")

    var players: List[(Int, Int)] = Nil

    for (i <- 0 until 2) {
      // numplayercards: the total number of quests for a player (hidden and revealed)
      val Array(_numplayercards, _playerx, _playery, playertile) = readLine split " "
      val numplayercards = _numplayercards.toInt
      val playerx = _playerx.toInt
      val playery = _playery.toInt
      players = (playerx, playery) :: players
//      Console.err.println("numplayercards: " + numplayercards)
//      Console.err.println("playerx: " + playerx)
//      Console.err.println("playery: " + playery)
//      Console.err.println("playertile: " + playertile)
//      Console.err.println("------------------------")
    }
//    Console.err.println("++++++++++++++++++++++++++++")
    val numitems = readInt // the total number of items available on board and on player tiles

    var items: List[(Int, Int, String)] = Nil

    for (i <- 0 until numitems) {
      val Array(itemname, _itemx, _itemy, _itemplayerid) = readLine split " "
      val itemx = _itemx.toInt
      val itemy = _itemy.toInt
      val itemplayerid = _itemplayerid.toInt
      if (itemplayerid == 0) items = ((itemx, itemy, itemname) :: items).filter(item => item._1 >=0 && item._2 >= 0)
//      Console.err.println("itemname: " + itemname)
//      Console.err.println("itemx: " + itemx)
//      Console.err.println("itemy: " + itemy)
//      Console.err.println("itemplayerid: " + itemplayerid)
//      Console.err.println("------------------------")
    }

//    val myItem = closest((players(1)._1, players(1)._2), items)

    val numquests = readInt // the total number of revealed quests for both players
    val quests = for (i <- 0 until numquests) yield readLine split " "
//    quests.flatten.foreach(Console.err.println)

//    for (i <- 0 until numquests) {
//      val Array(questitemname, _questplayerid) = readLine split " "
//      val questplayerid = _questplayerid.toInt
//      Console.err.println(s"questitemname: $questitemname\tquestplayerid: $questplayerid")
//      Console.err.println("------------------------")
//    }
//    val myItem = (1,2)
    val myItems = items.filter(_._3 == quests(0)(0))
    val myItem = if (myItems.isEmpty) closest((players(1)._1, players(1)._2), items.map(item => (item._1, item._2)))
                                      else (myItems.head._1, myItems.head._1)
//    Console.err.println(s"myItem2: $myItem")
    // Write an action using println
    // To debug: Console.err.println("Debug messages...")

//    inputs.foreach(a => {
//      Console.err.println()
//      a.foreach(b => Console.err.print(" " + b))
//    }
//    )
//      Console.err.println()
//      Console.err.println()
//    players.foreach(Console.err.println)
//      Console.err.println()
//      Console.err.println()
//    Console.err.println(myItem._1, myItem._2)
//      Console.err.println()
//      Console.err.println()

    val pb = possibleDirections((players(1)._1, players(1)._2), inputs(players(1)._2)(players(1)._1),
      Array(
        if (players(1)._2 - 1 >= 0) inputs(players(1)._2 - 1)(players(1)._1) else null,
        if (players(1)._1 + 1 <= 6) inputs(players(1)._2)(players(1)._1 + 1) else null,
        if (players(1)._2 + 1 <= 6) inputs(players(1)._2 + 1)(players(1)._1) else null,
        if (players(1)._1 - 1 >= 0) inputs(players(1)._2)(players(1)._1 - 1) else null
      )
    )
//    Console.err.println()
//    Console.err.println("Closest:" + closest(myItem, pb.keys.toList))


    val movetype = if (i % 2 == 0) "PUSH" else if (pb.isEmpty) "PASS" else "MOVE"

    var direction = if (movetype == "PUSH")
        if (players(1)._1 > myItem._1 && players(1)._2 != myItem._2) "LEFT" else
        if (players(1)._1 < myItem._1 && players(1)._2 != myItem._2) "RIGHT" else
        if (players(1)._2 < myItem._2 && players(1)._1 != myItem._1) "DOWN" else
        if (players(1)._2 > myItem._2 && players(1)._1 != myItem._1) "UP" else "DOWN"
      else if (movetype == "MOVE") {
        val graph = new Graph(49)
        for (i <- 0 until 6) {
          for (j <- 0 until 6) {
            if (connected(toNumber((i, j)), toNumber((i, j + 1)), inputs)) graph.addEdge(toNumber((i, j)), toNumber((i, j + 1)))
            if (connected(toNumber((i, j)), toNumber((i + 1, j)), inputs)) graph.addEdge(toNumber((i, j)), toNumber((i + 1, j)))
          }
        }
        val (edges, dists) = graph.bfs(toNumber(players(1)._1, players(1)._2))

//        Console.err.println(s"edges: ${edges(toNumber((myItem._1, myItem._2)))} to number: ${toNumber((myItem._1, myItem._2))}")
        if (edges(toNumber((myItem._1, myItem._2))) == Int.MaxValue) {
          pb(closest((myItem._1, myItem._2), pb.keys.toList))
        } else {
          path(toNumber(players(1)), toNumber((myItem._1, myItem._2)), edges)
        }
      } else if (movetype == "PASS") ""

//    Console.err.println("direction: " + direction + " turntype=" + turntype)
//    Console.err.println("movetype: " + movetype)
//    Console.err.println("I playesr: " + players(1))
    val rowid = if (movetype == "PUSH")
        (if (direction == "LEFT" || direction == "RIGHT") players(1)._2 else players(1)._1) + " "
//      (if (players(1)._1 != myItem._1) players(1)._1 else players(1)._2) + " "
    else ""


//    Console.err.println(s"$movetype $rowid $direction")
      println(s"$movetype $rowid$direction".trim) // PUSH <id> <direction> | MOVE <direction> | PASS

    i += 1
  }
}
