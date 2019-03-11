package codingames.challenge.codealamode

import scala.collection.mutable

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
//  case class Recipe (ingr: List[Map[String, Int]], f: List[Map[String, Int]] => List[Map[String, Int]])

  type Point = (Int, Int)
  type Recipe = List[Map[String, Int]]
  val iceCreamBlueberries = List("DISH", "BLUEBERRIES", "ICE_CREAM", "WINDOW")
  val iceCreamChoppedStrawberriesBlueberries = List("STRAWBERRIES", "CHOPPING", "DISH", "BLUEBERRIES", "WINDOW")
  val iceCreamChoppedStrawberries = List("STRAWBERRIES", "CHOPPING", "DISH", "BLUEBERRIES", "ICE_CREAM", "WINDOW")
  val cookBook = Map("DISH-BLUEBERRY-ICE_CREAM" -> iceCreamBlueberries,
                     "DISH-ICE_CREAM-CHOPPED_STRAWBERRIES-BLUEBERRIES" -> iceCreamChoppedStrawberriesBlueberries,
                     "DISH-ICE_CREAM-CHOPPED_STRAWBERRIES" -> iceCreamChoppedStrawberries)

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
    val myCustomerItems = customerItems.map(_ + "-WINDOW")
    Console.err.print(s"$playerItem")
    val playerPassedStages = if (playerItem == "NONE") Array.empty[String] else playerItem.split('-')
    val citems = myCustomerItems.map(_.split('-')(playerPassedStages.length)).distinct
    val filteredCitems = citems.filterNot(playerPassedStages.contains)
    Console.err.println(s" -> $filteredCitems")

//    val orderMap = Map("NONE" -> List("DISH"),
//      "DISH" -> List("BLUEBERRIES", "ICE_CREAM", "CHOPPED_STRAWBERRIES").filter(same),
//      "DISH-ICE_CREAM" -> List("BLUEBERRIES", "CHOPPED_STRAWBERRIES"),
//      "DISH-CHOPPED_STRAWBERRIES" -> List("BLUEBERRIES", "ICE_CREAM"),
//      "DISH-BLUEBERRIES" -> List("ICE_CREAM", "CHOPPED_STRAWBERRIES"),
//      "DISH-ICE_CREAM-BLUEBERRIES" -> List("WINDOW"),
//      "DISH-BLUEBERRIES-ICE_CREAM" -> List("WINDOW"),
//      "DISH-BLUEBERRIES-CHOPPED_STRAWBERRIES" -> List("WINDOW")
//    )
//    customerItems
//    orderMap(playerItem)
    filteredCitems
  }

  def createGraph(matrix: List[String]) = {
    val g = new Graph
    for (row <- matrix.indices; col <- matrix(row).indices; sym = matrix(row)(col); if walk(matrix, row, col)) {
//      Console.err.println(s"row=$row, col=$col, sym=$sym")
      if (walk(matrix, row - 1, col)) g.addEdge((col, row), (col, row - 1))
      if (walk(matrix, row, col - 1)) g.addEdge((col, row), (col - 1, row))
    }
    g
  }

  def reach(g: Graph, matrix: List[String], player: Point, target: List[Point]) = {
//    val (edgeTo, distTo) = g.bfs(player)
//    val targetPoints = target.flatMap(adjTo(matrix, _))/*.filter(p => distTo(p) < 5)*/
//    if (targetPoints.nonEmpty) "USE " + s"${targetPoints.head._1} ${targetPoints.head._2}" else "WAIT"
    s"USE ${target.head._1} ${target.head._2}"
  }

  val numallcustomers = readInt
  Console.err.println(s"numallcustomers: $numallcustomers")

  val customerData = (for (i <- 0 until numallcustomers) yield readLine split " ").toList

  val graphMatrix = (for (i <- 0 until 7) yield readLine).toList

  val targetMap = Map("DISH" -> searchSym(graphMatrix, 'D').getOrElse((-1,-1)),
                      "ICE_CREAM" -> searchSym(graphMatrix, 'S').getOrElse((-1,-1)),
                      "BLUEBERRIES" -> searchSym(graphMatrix, 'S').getOrElse((-1,-1)),
//                      "STRAWBERRY" -> searchSym(graphMatrix, 'S').getOrElse((-1,-1)),
                      "CHOPPING" -> searchSym(graphMatrix, 'C').getOrElse((-1,-1)),
                      "CHOPPED_STRAWBERRIES" -> searchSym(graphMatrix, 'S').getOrElse((-1,-1)),
                      "WINDOW" -> searchSym(graphMatrix, 'W').getOrElse((-1,-1)))

  // game loop
  while (true) {
    val turnsremaining = readInt
    Console.err.println(s"turnsremaining: $turnsremaining")
    val Array(_playerx, _playery, playeritem) = readLine split " "
    val playerx = _playerx.toInt
    val playery = _playery.toInt
    Console.err.println(s"playerx: $playerx, playery: $playery, playeritem: $playeritem")
    val Array(_partnerx, _partnery, partneritem) = readLine split " "
    val partnerx = _partnerx.toInt
    val partnery = _partnery.toInt
    Console.err.println(s"partnerx: $partnerx, partnery: $partnery, partneritem: $partneritem")
    val numtableswithitems = readInt // the number of tables in the kitchen that currently hold an item
    Console.err.println(s"numtableswithitems: $numtableswithitems")
    for (i <- 0 until numtableswithitems) {
      val Array(_tablex, _tabley, item) = readLine split " "
      val tablex = _tablex.toInt
      val tabley = _tabley.toInt
      Console.err.println(s"\ttablex: $tablex, tabley: $tabley")
    }
    // ovencontents: ignore until wood 1 league
    val Array(ovencontents, _oventimer) = readLine split " "
    val oventimer = _oventimer.toInt
    Console.err.println(s"ovencontents: $ovencontents, oventimer: $oventimer")
    val numcustomers = readInt // the number of customers currently waiting for food
    Console.err.println(s"numcustomers: $numcustomers")
    val customers = (for (i <- 0 until numcustomers) yield readLine split " ").toList.sortBy(_(0))
    customers.foreach(c => Console.err.println(s"customeritem: ${c(0)}, customeraward: ${c(1)}"))

    val g = createGraph(graphMatrix)
    val target = nextTarget(playeritem, customers.map(_(0)))
    Console.err.println(s"target is empty: ${target.isEmpty}")
    target.foreach(t => Console.err.println(s"TARGET: $t"))
    val action = reach(g, graphMatrix, (playerx, playery), target.map(targetMap.apply))
    println(action)
  }
}