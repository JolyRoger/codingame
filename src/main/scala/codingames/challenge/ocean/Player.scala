package codingames.challenge.ocean

import math._
import scala.collection.mutable
import scala.io.Source
import scala.util._
import scala.io.StdIn._

object Player extends App {

  type PathInfo = (String, Square)
  type OppPathUnit = (Square, List[PathInfo])
  type OppPath = Map[Square, List[PathInfo]]



  class Graph(board: Array[Array[Square]], flattenBoard: Array[Square]) {

  def allClosest(square: Square) = {
    val x = square.getX
    val y = square.getY

    List((x-1, y), (x, y-1), (x+1, y), (x, y+1)).withFilter { xy =>
      xy._1 >= 0 && xy._2 >= 0 && xy._1 < 15 && xy._2 < 15 && board(xy._2)(xy._1).water
    }.map( xy => board(xy._2)(xy._1))
  }

  val adj = (for (row <- 0 until 15; col <- 0 until 15; if board(col)(row).water) yield (board(col)(row), allClosest(board(col)(row)))).toMap
//    adj(toNumber((col, row))) = allClosest(col, row)

  def allDistance(from: Int) = {
    val (_, dist) = bfs(flattenBoard(from))
    dist
  }

  def distance(from: Int, to: Int) = {
    val (_, dist) = bfs(flattenBoard(from))
    dist(to)
  }

  def bfs(s: Square) = {
    val N = 15 * 15
    val marked = Array.fill[Boolean](N)(false)
    val edgeTo = Array.fill[Int](N)(Int.MaxValue)
    val distTo = Array.fill[Int](N)(Int.MaxValue)
    val q = mutable.Queue[Square]()
    var i = 0

    q.enqueue(s)
    marked(s.index) = true
    distTo(s.index) = 0

    while (q.nonEmpty) {
      val v = q.dequeue
      i = i + 1
      adj(v).filterNot(sq => marked(sq.index)).foreach(
        w => {
          q.enqueue(w)
          marked(w.index) = true
          edgeTo(w.index) = v.index
          distTo(w.index) = distTo(v.index) + 1
        }
      )
    }
    (edgeTo, distTo)
  }
}

  class Square(x: Int, y: Int, sym: Char) {
    val index = y * 15 + x
    val getX = x
    val getY = y
    val getSym = sym
    val water = sym == '.'
    var accessible = water

    val allTorpedoSquares = calcSquare(x, y)

    private def calc(x: Int, y: Int, offset: Int, leftLimit: Int, rightLimit: Int) = {
      val internalOffset = 4 - Math.abs(offset)
      val a = (x - internalOffset to x).dropWhile(_ < leftLimit).toSet
      val b = (x to x + internalOffset).takeWhile(_ <= rightLimit).toSet
      (a ++ Set(x) ++ b).map((_, y - offset)).filter(pair => pair._2 >= 0 && pair._2 < 15)
    }

    private def calcSquare(x: Int, y: Int) = {
      (for (offset <- -4 to 4) yield calc(x, y, offset, 0, 14)).toSet.flatten
    }

    def print = Console.err.print(s"$sym")
  //  override def toString = index.toString
    override def toString = s"[$x,$y]$sym"
  }

  abstract class SquareManager(legalSquares: Array[Square], flattenBoard: Array[Square]) {
    val torpedoSquareMap = legalSquares.map(square => (square, square.allTorpedoSquares.filter(xy => flattenBoard(xy._2 * 15 + xy._1).water))).toMap
  }

  class OppSquareManager(legalSquares: Array[Square], flattenBoard: Array[Square]) extends SquareManager(legalSquares, flattenBoard) {
    val oppositeDirection = Map("S" -> "N", "N" -> "S", "W" -> "E", "E" -> "W")
    val emptyOppPath = Map.empty[Square, List[Square]]
    val sector = Array ((for (x <- 0 until 5; y <- 0 until 5) yield flattenBoard(y * 15 + x)).filter(_.water).toArray,
                        (for (x <- 5 until 10; y <- 0 until 5) yield flattenBoard(y * 15 + x)).filter(_.water).toArray,
                        (for (x <- 10 until 15; y <- 0 until 5) yield flattenBoard(y * 15 + x)).filter(_.water).toArray,
                        (for (x <- 0 until 5; y <- 5 until 10) yield flattenBoard(y * 15 + x)).filter(_.water).toArray,
                        (for (x <- 5 until 10; y <- 5 until 10) yield flattenBoard(y * 15 + x)).filter(_.water).toArray,
                        (for (x <- 10 until 15; y <- 5 until 10) yield flattenBoard(y * 15 + x)).filter(_.water).toArray,
                        (for (x <- 0 until 5; y <- 10 until 15) yield flattenBoard(y * 15 + x)).filter(_.water).toArray,
                        (for (x <- 5 until 10; y <- 10 until 15) yield flattenBoard(y * 15 + x)).filter(_.water).toArray,
                        (for (x <- 10 until 15; y <- 10 until 15) yield flattenBoard(y * 15 + x)).filter(_.water).toArray)

    def nextRawSquare(currentSquare: Square, direction: String) = {
      takeRawSquare(currentSquare, direction, 1)
    }

    def takeRawSquare(currentSquare: Square, direction: String, step: Int) = {
      direction match {
        case "N" => Try(flattenBoard((currentSquare.getY - step) * 15 + currentSquare.getX))
        case "S" => Try(flattenBoard((currentSquare.getY + step) * 15 + currentSquare.getX))
        case "W" => Try(flattenBoard((currentSquare.getY) * 15 + currentSquare.getX - step))
        case "E" => Try(flattenBoard((currentSquare.getY) * 15 + currentSquare.getX + step))
      }
    }

    def takeSquares(currentSquare: OppPathUnit, direction: String) = {
      (for (step <- 1 to 4) yield {
          takeRawSquare(currentSquare._1, direction, step)
      }).takeWhile(square => square.isSuccess && square.get.water).map(successSquare =>
        (successSquare.get, (direction, currentSquare._1) :: currentSquare._2)).toSet
    }

    def processOpponentMove(opponentOrders: String, oppPath: OppPath) = {
      val Array(_, direction) = opponentOrders.split("\\s")
      oppPath.map {sqPath =>
        val ns = nextRawSquare(sqPath._1, direction)
        val nl = (direction, sqPath._1) :: sqPath._2
        (ns, nl)
      }.withFilter(sqMap => sqMap._1 match {
        case Success(sq) => sq.water
        case Failure(_) => false
      }).map(sqMap => (sqMap._1.get, sqMap._2))
    }

    def processOpponentSurface(opponentOrders: String, oppPath: OppPath) = {
      val Array(_, sec) = opponentOrders.split("\\s")
      oppPath.keys.toArray.intersect(sector(sec.toInt - 1)).map((_, List.empty[PathInfo])).toMap
    }

    def processOpponentSilence(oppSquares: OppPath) = {
      val extraSquares = oppSquares.flatMap(squarePath => {
        val lastMove = squarePath._2.headOption.getOrElse(("", null))._1
        val r = (Set("N", "S", "W", "E") - lastMove).flatMap(dir => takeSquares(squarePath, oppositeDirection(dir)))
        r
  //      takeSquares(square, "N") ++ takeSquares(square, "S") ++ takeSquares(square, "W") ++ takeSquares(square, "E")
      }).filter(_._1.water)
      extraSquares ++ oppSquares
    }
  }

  class MySquareManager(board: Array[Array[Square]],
                        legalSquares: Array[Square],
                        flattenBoard: Array[Square]) extends SquareManager(legalSquares, flattenBoard) {

    val rand = new Random(System.currentTimeMillis)
    var myPosition = legalSquares(rand.nextInt(legalSquares.length))
    Console.err.println(s"my position=$myPosition")


    val safeTorpedoSquareMap = torpedoSquareMap.map(kv => (kv._1, kv._2.filter { xy =>
      (Math.abs(kv._1.getX - xy._1) > 1 || Math.abs(kv._1.getY - xy._2) > 1) && flattenBoard(xy._2 * 15 + xy._1).water
    }))

    myPosition.accessible = false

    def safeTorpedoSquares = safeTorpedoSquareMap(myPosition)

    def surface = {
      legalSquares.foreach(square => square.accessible = true)
      myPosition.accessible = false
      ""
    }

    def setMyPosition(square: Square) {
      myPosition = square
      myPosition.accessible = false
    }


    def possibleDirection = {
      val candidates = Array((myPosition.getX + 1, myPosition.getY, 'E'), (myPosition.getX - 1, myPosition.getY, 'W'), (myPosition.getX, myPosition.getY + 1, 'S'), (myPosition.getX, myPosition.getY - 1, 'N'))
      candidates.withFilter(xy => xy._1 >= 0 && xy._1 < 15 &&
                              xy._2 >= 0 && xy._2 < 15 &&
                              board(xy._2)(xy._1).accessible)
        .map(data => (board(data._2)(data._1), data._3))
    }
  }


//------------------------------------------FILE ENTRY------------------------------------------------------------------
//  val filename = "ocean/ocean0.txt"
//  val bufferedSource = Source.fromFile(filename)
//  val data = bufferedSource.getLines
//
//  def readInt = if (data.hasNext) data.next.toInt else -1
//  def readLine = if (data.hasNext) data.next else "EOF"
//----------------------------------------------------------------------------------------------------------------------

  val Array(width, height, myId) = (readLine split " ").map(_.toInt)
  Console.err.println(s"myId=$myId size=$width:$height")

  val boardSym = (for (i <- 0 until height) yield readLine).map(_.toCharArray)
  val board = boardSym.zipWithIndex.map(arrIndex => arrIndex._1.zipWithIndex.map(
    symIndex => new Square(symIndex._2, arrIndex._2, symIndex._1))).toArray
  val flattenBoard = board.flatten
  val squareArray = flattenBoard
  val coordSquaresMap = flattenBoard.map(square => ((square.getX, square.getY), square)).toMap
  val legalSquares = flattenBoard.filter(_.water)

  val myManager = new MySquareManager(board, legalSquares, flattenBoard)
  val oppManager = new OppSquareManager(legalSquares, flattenBoard)
  val graph = new Graph(board, flattenBoard)

  val oppLegalSquaresMap = legalSquares.map((_, List.empty[PathInfo])).toMap
  var oppSquares = oppLegalSquaresMap

  board.foreach(bl => {
    bl.foreach(_.print)
    Console.err.println})

  println(s"${myManager.myPosition.getX} ${myManager.myPosition.getY}")
//  println("0 6")

  while (true) {
    val Array(x, y, myLife, oppLife, torpedoCooldown, sonarCooldown, silenceCooldown, mineCooldown) = (readLine split " ").map(_.toInt)
    myManager.setMyPosition(board(y)(x))
    Console.err.println(s"$x $y $myLife $oppLife $torpedoCooldown $sonarCooldown $silenceCooldown $mineCooldown")
    val sonarResult = readLine
    Console.err.println(s"sonarResult=$sonarResult")
    val opponentOrders = readLine
    Console.err.println(s"opponentOrders=$opponentOrders")
    val opponentOrdersArr = opponentOrders.split("\\|")

    oppSquares = opponentOrdersArr.map { oppOrder =>
      val order = oppOrder.trim
      Console.err.println(s"ORDER: $order")
      if (order.startsWith("MOVE")) oppManager.processOpponentMove(order, oppSquares)
      else if (order.startsWith("SURFACE")) oppManager.processOpponentSurface(order, oppSquares)
      else if (order.startsWith("SILENCE")) oppManager.processOpponentSilence(oppSquares)
      else if (order.startsWith("TORPEDO")) oppSquares
      else if (order.startsWith("NA")) oppLegalSquaresMap
      else oppSquares
    }.reduce((a,b) => a ++ b)   // FIXME

    if (oppSquares.isEmpty) Console.err.println("!!!!!!!!!!ERROR!!!!!!!!!!!!!")
    Console.err.println(s"oppSquares.size=${oppSquares.size}")

    oppSquares.keys.toList.sortBy(_.index).foreach(Console.err.print)

    val directions = myManager.possibleDirection
//    val nextMove = directions.map(_._1).minBy(_.)
    val (mainCommand, newpos) = if (directions.isEmpty) ("SURFACE", myManager.myPosition) else {
      val newpos = directions(myManager.rand.nextInt(directions.length))
      (s"MOVE ${newpos._2} TORPEDO", newpos._1)
    }

    val dopCommand = if (mainCommand == "SURFACE") {
      myManager.surface
    } else if (mainCommand.startsWith("MOVE")) {
      s"${if (torpedoCooldown > 0) "" else {
        val torpedoSquares = myManager.safeTorpedoSquareMap(newpos)
        val nearOppCandidateCoord = torpedoSquares.intersect(oppSquares.keySet.map(sq => (sq.getX, sq.getY)))
        nearOppCandidateCoord.find(_ => true) match {
          case Some(s) => {
            s"|TORPEDO ${s._1} ${s._2}"
          }
          case None => ""
        }
      }}"
    }

    println(s"$mainCommand$dopCommand")
//    println(s"MOVE E SILENCE TORPEDO SONAR")
  }
}