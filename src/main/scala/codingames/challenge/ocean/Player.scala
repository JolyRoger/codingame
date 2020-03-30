package codingames.challenge.ocean

import math._
import scala.collection.mutable
import scala.io.Source
import scala.util._
import scala.io.StdIn._

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
  override def toString = index.toString
//  override def toString = s"($x,$y)$sym"
}

class OppSquareManager(legalSquares: Array[Square], flattenBoard: Array[Square]) {

  type OppPath = Map[Square, List[Square]]

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



  def nextSquare(currentSquare: Square, direction: String) = {
    direction match {
      case "N" => Try(flattenBoard((currentSquare.getY - 1) * 15 + currentSquare.getX))
      case "S" => Try(flattenBoard((currentSquare.getY + 1) * 15 + currentSquare.getX))
      case "W" => Try(flattenBoard((currentSquare.getY) * 15 + currentSquare.getX - 1))
      case "E" => Try(flattenBoard((currentSquare.getY) * 15 + currentSquare.getX + 1))
    }
  }

  def processOpponentMove(opponentOrders: String, oppPath: OppPath) = {
    val Array(_, direction) = opponentOrders.split("\\s")
    oppPath.map {sqPath =>
      val ns = nextSquare(sqPath._1, direction)
      val nl = sqPath._1 :: sqPath._2
      (ns, nl)
    }.withFilter(sqMap => sqMap._1 match {
      case Success(sq) => sq.water
      case Failure(_) => false
    }).map(sqMap => (sqMap._1.get, sqMap._2))
  }

  def processOpponentSurface(opponentOrders: String, oppPath: OppPath) = {
    val Array(_, sec) = opponentOrders.split("\\s")
    oppPath.keys.toArray.intersect(sector(sec.toInt - 1)).map((_, List.empty[Square])).toMap
  }
}

class MySquareManager(board: Array[Array[Square]],
                      flattenBoard: Array[Square],
                      legalSquares: Array[Square]) {
  val rand = new Random(System.currentTimeMillis)
//  var myPosition = coordSquaresMap((0, 8))
  var myPosition = legalSquares(rand.nextInt(legalSquares.length))
  Console.err.println(s"my position=$myPosition")

  val torpedoSquareMap = legalSquares.map(square => (square, square.allTorpedoSquares.filter(xy => flattenBoard(xy._2 * 15 + xy._1).water))).toMap
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
      .map(_._3)
  }
}

object Player extends App {
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

  val myManager = new MySquareManager(board, flattenBoard, legalSquares)
  val oppManager = new OppSquareManager(legalSquares, flattenBoard)
  val graph = new Graph(board, flattenBoard)

  val oppLegalSquaresMap = legalSquares.map((_, List.empty[Square])).toMap
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

    oppSquares = opponentOrdersArr.map { oo =>
      val order = oo.trim
      Console.err.println(s"ORDER: $order")
      if (order.startsWith("MOVE")) oppManager.processOpponentMove(order, oppSquares)
      else if (order.startsWith("SURFACE")) oppManager.processOpponentSurface(order, oppSquares)
      else if (order.startsWith("TORPEDO")) oppManager.emptyOppPath
      else if (order.startsWith("NA")) oppLegalSquaresMap
      else oppManager.emptyOppPath
    }.reduce(_ ++ _)

    Console.err.println(s"MOVE::${oppSquares.keys.mkString(" ")}")

    val directions = myManager.possibleDirection
    val mainCommand = if (directions.isEmpty) "SURFACE" else s"MOVE ${directions(myManager.rand.nextInt(directions.length))} TORPEDO"

    val dopCommand = if (mainCommand == "SURFACE") {
      myManager.surface
    } else if (mainCommand.startsWith("MOVE")) {
      s"${if (torpedoCooldown > 0) "" else {
        val torpedoSquares = myManager.safeTorpedoSquares
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