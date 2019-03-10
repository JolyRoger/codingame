//package codingames.challenge.codealamode

import scala.collection.mutable

object MyState extends Enumeration {
  type Margin = Value
  val EMPTY, WITH_EMPTY_PLATE, WITH_BLUEBERRY_PLATE, WITH_ICECREAM_PLATE, WITH_BOTH_PLATE = Value
}

class Graph {
  val N = 7 * 11
  val adj = (for (i <- 0 until N) yield List[Int]()).toArray
  def addEdge(v: Int, w: Int){
    adj(v) = w :: adj(v)
    adj(w) = v :: adj(w)
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

object Player extends App {
  type Point = (Int, Int)
  implicit def toNumber(point: Point): Int = point._2 * 11 + point._1 % 11
  def toMatrix(number: Int): Point = (number % 11, number / 11)
  def walk(matrix: List[String], row: Int, col: Int) = row > 0 && row < 7 && col > 0 && col < 11 && (matrix(row)(col) == '.' || matrix(row)(col) == '0')
  def replaceSym(matrix: List[String], oldSym: Char, newSym: Char) = matrix.map(_.replace(oldSym, newSym))
  def searchSym(matrix: List[String], sym: Char) = {
    val coords = for (row <- matrix.indices; col <- matrix(row).indices; _sym = matrix(row)(col); if _sym == sym) yield (col, row)
    if (coords.nonEmpty) Some(coords(0)) else None
  }
  def adjTo(matrix: List[String], point: Point): List[Point] = List((point._1 - 1, point._2 - 1), (point._1, point._2 - 1),
                   (point._1 + 1, point._2 - 1), (point._1 - 1, point._2), (point._1 + 1, point._2), (point._1 - 1, point._2 + 1),
                   (point._1, point._2 + 1), (point._1 + 1, point._2 + 1)).filter(p => walk(matrix, p._2, p._1))
  def nextTarget(playerItem: String, customerItems: List[String]): List[String] = {
      def same(component: String) = customerItems.map(_.split('-')(1)).distinct.contains(component)
      val orderMap = Map("NONE" -> List("DISH"),
        "DISH" -> List("BLUEBERRIES", "ICE_CREAM").filter(same),
        "ICE_CREAM" -> List("BLUEBERRIES"),
        "DISH-ICE_CREAM" -> List("BLUEBERRIES"),
        "DISH-BLUEBERRIES" -> List("ICE_CREAM"),
        "DISH-ICE_CREAM-BLUEBERRIES" -> List("WINDOW"),
        "DISH-BLUEBERRIES-ICE_CREAM" -> List("WINDOW")
      )
      orderMap(playerItem)
    }

  def createGraph(matrix: List[String]) = {
    val g = new Graph
    for (row <- matrix.indices; col <- matrix(row).indices; sym = matrix(row)(col); if walk(matrix, row, col)) {
      if (walk(matrix, row - 1, col)) g.addEdge((col, row), (col, row - 1))
      if (walk(matrix, row, col - 1)) g.addEdge((col, row), (col - 1, row))
    }
    g
  }

  def reach(g: Graph, matrix: List[String], player: Point, target: List[Point]) = s"USE ${target.head._1} ${target.head._2}"

  val numallcustomers = readInt
  val customerData = (for (i <- 0 until numallcustomers) yield readLine split " ").toList
  val graphMatrix = (for (i <- 0 until 7) yield readLine).toList
  val targetMap = Map("DISH" -> searchSym(graphMatrix, 'D').getOrElse((-1,-1)),
                      "ICE_CREAM" -> searchSym(graphMatrix, 'I').getOrElse((-1,-1)),
                      "BLUEBERRIES" -> searchSym(graphMatrix, 'B').getOrElse((-1,-1)),
                      "WINDOW" -> searchSym(graphMatrix, 'W').getOrElse((-1,-1)))

  // game loop
  while (true) {
    val turnsremaining = readInt
    val Array(_playerx, _playery, playeritem) = readLine split " "
    val playerx = _playerx.toInt
    val playery = _playery.toInt
    val Array(_partnerx, _partnery, partneritem) = readLine split " "
    val partnerx = _partnerx.toInt
    val partnery = _partnery.toInt
    val numtableswithitems = readInt
    for (i <- 0 until numtableswithitems) {
      val Array(_tablex, _tabley, item) = readLine split " "
      val tablex = _tablex.toInt
      val tabley = _tabley.toInt
    }
    val Array(ovencontents, _oventimer) = readLine split " "
    val oventimer = _oventimer.toInt
    val numcustomers = readInt
    val customers = (for (i <- 0 until numcustomers) yield readLine split " ").toList.sortBy(_(0))

    val g = createGraph(graphMatrix)
    val target = nextTarget(playeritem, customers.map(_(0)))
    val action = reach(g, graphMatrix, (playerx, playery), target.map(targetMap.apply))

    println(action)
  }
}