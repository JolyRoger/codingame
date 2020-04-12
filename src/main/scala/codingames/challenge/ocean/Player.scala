//package codingames.challenge.ocean

import math._
import scala.io.Source
import scala.util._
import scala.io.StdIn._

object Player extends App {

  type PathInfo = (String, Square)
  type OppPathUnit = (Square, List[PathInfo])
  type OppPath = Map[Square, List[PathInfo]]

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

  class OppSquareManager(board: Array[Array[Square]], legalSquares: Array[Square], flattenBoard: Array[Square]) extends SquareManager(legalSquares, flattenBoard) {
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

    def nextRawSquare(square: Square) = {
      Set(
        Try(board(square.getY - 1)(square.getX)),
        Try(board(square.getY - 1)(square.getX + 1)),
        Try(board(square.getY - 1)(square.getX - 1)),
        Try(board(square.getY + 1)(square.getX)),
        Try(board(square.getY + 1)(square.getX + 1)),
        Try(board(square.getY + 1)(square.getX - 1)),
        Try(board(square.getY)(square.getX - 1)),
        Try(board(square.getY)(square.getX + 1)),
      ).collect {
        case t if t.isSuccess => t.get
      }
    }

    def nextRawSquareTry(currentSquare: Square, direction: String) = {
      takeRawSquareTry(currentSquare, direction, 1)
    }

    def takeRawSquareTry(currentSquare: Square, direction: String, step: Int) = {
      direction match {
        case "N" => Try(board(currentSquare.getY - step)(currentSquare.getX))
        case "S" => Try(board(currentSquare.getY + step)(currentSquare.getX))
        case "W" => Try(board(currentSquare.getY)(currentSquare.getX - step))
        case "E" => Try(board(currentSquare.getY)(currentSquare.getX + step))
      }
    }

    def takeSquares(currentSquare: OppPathUnit, direction: String) = {
      (for (step <- 1 to 4) yield {
          takeRawSquareTry(currentSquare._1, direction, step)
      }).takeWhile(square => square.isSuccess && square.get.water).map(successSquare =>
        (successSquare.get, (direction, currentSquare._1) :: currentSquare._2)).toSet
    }

    def processOpponentMove(opponentOrders: String, oppPath: OppPath) = {
      val Array(_, direction) = opponentOrders.split("\\s")
      val res = oppPath.map {sqPath =>
        val ns = nextRawSquareTry(sqPath._1, direction)
        val nl = (direction, sqPath._1) :: sqPath._2
        (ns, nl)
      }.withFilter(sqMap => sqMap._1 match {
        case Success(sq) => sq.water
        case Failure(_) => false
      }).map(sqMap => (sqMap._1.get, sqMap._2))
      res
    }

    def processOpponentSurface(opponentOrders: String, oppPath: OppPath) = {
      val Array(_, sec) = opponentOrders.split("\\s")
      oppPath.keys.toArray.intersect(sector(sec.toInt - 1)).map((_, List.empty[PathInfo])).toMap
    }

    def processOpponentSilence(oppSquares: OppPath) = {
      val extraSquares = oppSquares.flatMap(squarePath => {
        val lastMove = squarePath._2.headOption.getOrElse(("", null))._1
        (Set("N", "S", "W", "E") - lastMove).flatMap(dir => takeSquares(squarePath, oppositeDirection(dir)))
      }).filter(_._1.water)
      extraSquares ++ oppSquares
    }
  }

  class MySquareManager(board: Array[Array[Square]],
                        legalSquares: Array[Square],
                        flattenBoard: Array[Square]) extends SquareManager(legalSquares, flattenBoard) {
    val rand = new Random(System.currentTimeMillis)
    var myPosition = legalSquares(rand.nextInt(legalSquares.length))
    // Console.err.println(s"my position=$myPosition")

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
/*
  val filename = "ocean/ocean0.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines

  def readInt = if (data.hasNext) data.next.toInt else -1
  def readLine = if (data.hasNext) data.next else "EOF"
*/
//----------------------------------------------------------------------------------------------------------------------

  def euclidean(a: (Int, Int), b: (Int, Int)) = sqrt(pow(b._1 - a._1, 2) + pow(b._2 - a._2, 2))
  def valid(square: Square) = square.accessible
  val Array(width, height, myId) = (readLine split " ").map(_.toInt)
//   Console.err.println(s"myId=$myId size=$width:$height")

  val boardSym = (for (i <- 0 until height) yield readLine).map(_.toCharArray)
  val board = boardSym.zipWithIndex.map(arrIndex => arrIndex._1.zipWithIndex.map(
    symIndex => new Square(symIndex._2, arrIndex._2, symIndex._1))).toArray
  val flattenBoard = board.flatten
  val squareArray = flattenBoard
  val coordSquaresMap = flattenBoard.map(square => ((square.getX, square.getY), square)).toMap
  val legalSquares = flattenBoard.filter(_.water)
  val legalSquareSize = legalSquares.length
  var euclideanDistanceMap = Map.empty[(Square, Square), Double]

  for (square1 <- legalSquares; square2 <- legalSquares; if !euclideanDistanceMap.contains((square1, square2))) {
    val dist = euclidean((square1.getX, square1.getY), (square2.getX, square2.getY))
    euclideanDistanceMap = euclideanDistanceMap + ((square1, square2) -> dist)
    euclideanDistanceMap = euclideanDistanceMap + ((square2, square1) -> dist)
  }

  val myManager = new MySquareManager(board, legalSquares, flattenBoard)
  val oppManager = new OppSquareManager(board, legalSquares, flattenBoard)
  val oppLegalSquaresMap = legalSquares.map((_, List.empty[PathInfo])).toMap
  var oppSquares = oppLegalSquaresMap

//  board.foreach(bl => {
//    bl.foreach(_.print)
//     Console.err.println})

  println(s"${myManager.myPosition.getX} ${myManager.myPosition.getY}")
//  println("0 6")

  def findSquareAsOppPath(oppSquares: OppPath) = oppSquares.find(square => square._1.index == board(torpedoCoord._2)(torpedoCoord._1).index) match {
    case Some(sq) => Map(sq._1 -> sq._2)
    case None => oppSquares
  }

  def processOppMove(oppSquares: OppPath, oppOrder: String) = {
      val order = oppOrder.trim
//       Console.err.println(s"ORDER: $order")
      if (order.startsWith("MOVE")) oppManager.processOpponentMove(order, oppSquares)
      else if (order.startsWith("SURFACE")) oppManager.processOpponentSurface(order, oppSquares)
      else if (order.startsWith("SILENCE")) oppManager.processOpponentSilence(oppSquares)
      else if (order.startsWith("TORPEDO")) oppSquares
      else if (order.startsWith("NA")) oppLegalSquaresMap
      else oppSquares
  }

  def processOppMoves(oppSquares: OppPath, opponentOrders: Array[String]) = {
    var opps = oppSquares
    opponentOrders.foreach { order =>
      opps = processOppMove(opps, order)
    }
    opps
  }

  var oppPrevLife = 6
  var torpedoRun = false
//  var torpedoRun = true
  var torpedoCoord = (-1,-1)
//  var torpedoCoord = (3,4)

  while (true) {
    val Array(x, y, myLife, oppLife, torpedoCooldown, sonarCooldown, silenceCooldown, mineCooldown) = (readLine split " ").map(_.toInt)
    myManager.setMyPosition(board(y)(x))
    // Console.err.println(s"$x $y $myLife $oppLife $torpedoCooldown $sonarCooldown $silenceCooldown $mineCooldown")
    val sonarResult = readLine
    // Console.err.println(s"sonarResult=$sonarResult")
    val opponentOrders = readLine
    // Console.err.println(s"opponentOrders=$opponentOrders")
    val opponentOrdersArr = opponentOrders.split("\\|")

    oppSquares = if (torpedoRun) {
      if (oppLife < oppPrevLife) {
//        Console.err.println(s"torpedoCoord=${torpedoCoord} oppLife=$oppLife oppPrevLife=$oppPrevLife")
        if (oppPrevLife - oppLife == 2) {
          val oppsq = oppSquares.filter(sqpth => sqpth._1.index == board(torpedoCoord._2)(torpedoCoord._1).index)
//          Console.err.println(s"torpedo 2")
//          oppsq.keys.toList.sortBy(_.index).foreach(Console.err.print)
//          Console.err.println
          oppsq
        } else if (oppPrevLife - oppLife == 1) {
//          Console.err.println(s"torpedo 1")
          val squares = oppManager.nextRawSquare(board(torpedoCoord._2)(torpedoCoord._1))
          val oppsq = oppSquares.filter(sqpth => squares.contains(sqpth._1))
//          oppsq.keys.toList.sortBy(_.index).foreach(Console.err.print)
//          Console.err.println
          oppsq
        } else {
          Console.err.println(s"Double hit!") // FIXME
          oppSquares
        }
      } else {
          val squares = oppManager.nextRawSquare(board(torpedoCoord._2)(torpedoCoord._1)) + board(torpedoCoord._2)(torpedoCoord._1)
          val oppsq = oppSquares.filterNot(sqpth => squares.contains(sqpth._1))
//          Console.err.println(s"Loose. New size= ${oppsq.size}")
//          oppsq.keys.toList.sortBy(_.index).foreach(Console.err.print)    ,.
//          Console.err.println1
          oppsq
      }
    } else oppSquares

    oppSquares = processOppMoves(oppSquares, opponentOrdersArr)
//    oppSquares.keys.toList.sortBy(_.index).foreach(Console.err.print)

    val oppSize = oppSquares.size
    if (oppSquares.isEmpty) Console.err.println("!!!!!!!!!!ERROR!!!!!!!!!!!!!")
//    Console.err.println(s"oppSquares.size=$oppSize")
//    if (oppSize < 30) oppSquares.keys.toList.sortBy(_.index).foreach(Console.err.print)

/*
    val (edges, allDist) = graph.allDistance(myManager.myPosition.index, valid)
    val dist = allDist.indices.filter(index => allDist(index) < Int.MaxValue).toList
    val inds = oppSquares.keys.map(_.index).toList
    val res = dist.intersect(inds)

    // Console.err.println(s"$res")
*/

    val directions = myManager.possibleDirection
    val oppsq = oppSquares.keySet

/*
    val p = graph.path(34, 16, edges)
    val go = p.tail.head
    // Console.err.println(s"$go")

//    val nextMove = directions. /.map(_._1).minBy(_.)
*/


    val (mainCommand, newpos) = if (directions.isEmpty) ("SURFACE", myManager.myPosition) else {
      val dir = directions.map(sqch => {
        val minOpp = oppsq.minBy(s => euclideanDistanceMap((s, sqch._1)))
        (euclideanDistanceMap((minOpp, sqch._1)), sqch._1, sqch._2)
      }).minBy(_._1)
      // Console.err.println(s"$dir")
//      val newpos = directions(myManager.rand.nextInt(directions.size))
      (s"MOVE ${dir._3}", dir._2)
    }

    torpedoRun = false

    val dopCommand = if (mainCommand == "SURFACE") {
      myManager.surface
    } else if (mainCommand.startsWith("MOVE")) {
      s"${if (torpedoCooldown > 0/* || oppSquares.size > 70*/) "" else {
        val unsafeTorpedoSquares = myManager.torpedoSquareMap(newpos)
        val safeTorpedoSquares = myManager.safeTorpedoSquareMap(newpos)
        val oppCoord = oppsq.map(sq => (sq.getX, sq.getY))
        val torpedoSquares = if (oppCoord.size == 1 && myLife >= oppLife) unsafeTorpedoSquares else safeTorpedoSquares
        val nearOppCandidateCoord = torpedoSquares.intersect(oppCoord)
        nearOppCandidateCoord.find(_ => true) match {
          case Some(s) => {
            torpedoRun = true
            oppPrevLife = oppLife
            torpedoCoord = s
            s"|TORPEDO ${s._1} ${s._2}"
          }
          case None => ""
        }
      }}"
    }

    def load = if (torpedoCooldown > 0) " TORPEDO" else
                 if (silenceCooldown > 0) " SILENCE" else
                 if (sonarCooldown > 0) " SONAR" else
                 if (mineCooldown > 0) " MINE" else ""

    println(s"$mainCommand$load$dopCommand")
  }
}