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

    def print = Console.err.print(s"$sym")
  //  override def toString = index.toString
    override def toString = s"[$x,$y]$sym"
  }

  abstract class SquareManager(board: Array[Array[Square]], legalSquares: Array[Square], flattenBoard: Array[Square]) {
//    val torpedoSquareMap = legalSquares.map(square => (square, square.allTorpedoSquares.filter(xy => flattenBoard(xy._2 * 15 + xy._1).water))).toMap
    val oppositeDirection = Map("S" -> "N", "N" -> "S", "W" -> "E", "E" -> "W", "" -> "")
    val directions = Set("N", "W", "S", "E")

    def takeRawSquareTry(currentSquare: Square, direction: String, step: Int) = {
      direction match {
        case "N" => Try(board(currentSquare.getY - step)(currentSquare.getX))
        case "S" => Try(board(currentSquare.getY + step)(currentSquare.getX))
        case "W" => Try(board(currentSquare.getY)(currentSquare.getX - step))
        case "E" => Try(board(currentSquare.getY)(currentSquare.getX + step))
      }
    }

    def nextRawSquareTry(currentSquare: Square, direction: String) = {
      takeRawSquareTry(currentSquare, direction, 1)
    }

    def getNextCrossSquares(currentSquare: Square, step: Int) = {
      directions.flatMap { direction =>
        val res = (for (i <- 1 to step) yield takeRawSquareTry(currentSquare, direction, i)).collect { case ts if ts.isSuccess => ts.get }
        res.toSet
      }
    }

    def takeSilenceSquares(currentSquare: OppPathUnit, direction: String, valid: Square => Boolean) = {
      (for (step <- 1 to 4) yield {
        takeRawSquareTry(currentSquare._1, direction, step)
      }).takeWhile(square => square.isSuccess && valid(square.get)).map(successSquare =>
        (successSquare.get, (direction, currentSquare._1) :: currentSquare._2)).toSet
    }
  }

  class OppSquareManager(board: Array[Array[Square]], legalSquares: Array[Square], flattenBoard: Array[Square]) extends SquareManager(board, legalSquares, flattenBoard) {
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

    def processOpponentMove(opponentOrders: String, oppPath: OppPath) = {
      val Array(_, direction) = opponentOrders.split("\\s")
      oppPath.map {sqPath =>
        val ns = nextRawSquareTry(sqPath._1, direction)
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
        (directions - lastMove).flatMap(dir => takeSilenceSquares(squarePath, oppositeDirection(dir), _.water))
      }).filter(_._1.water)
      extraSquares ++ oppSquares
    }
  }

  class MySquareManager(board: Array[Array[Square]],
                        legalSquares: Array[Square],
                        flattenBoard: Array[Square],
                        distanceMap: Map[(Square, Square), Double]) extends SquareManager(board, legalSquares, flattenBoard) {
    val rand = new Random(System.currentTimeMillis)
    var myPosition = legalSquares(rand.nextInt(legalSquares.length))
//    var myPosition = board(0)(0)

    val (safeTorpedoSquareMap, unsafeTorpedoSquareMap) = {
      val unsafeSquares = legalSquares.map(square => (square, calculateUnsafeTorpedoSquaresStack(square))).toMap
      val safeSquares = unsafeSquares.map(kv => (kv._1, kv._2.filter(v => distanceMap((kv._1, v)) > 1.5)))
      (safeSquares, unsafeSquares)
    }

    myPosition.accessible = false

    private def calculateUnsafeTorpedoSquaresStack(currentSquare: Square) = {
      var stack = Set(currentSquare)

      for (i <- 1 to 4) {
            val neighbours = stack.flatMap {
              square => directions.map(takeRawSquareTry(square, _, 1)).collect { case s if s.isSuccess && s.get.water => s.get }
            }
            stack = neighbours ++ stack
      }
      stack
    }

    def surface = {
      legalSquares.foreach(square => square.accessible = true)
      myPosition.accessible = false
      ""
    }

    def setMyPosition(square: Square) {
      if (square.getX < myPosition.getX) for (x <- square.getX to myPosition.getX) board(square.getY)(x).accessible = false
      if (square.getX > myPosition.getX) for (x <- myPosition.getX to square.getX) board(square.getY)(x).accessible = false
      if (square.getY < myPosition.getY) for (y <- square.getY to myPosition.getY) board(y)(square.getX).accessible = false
      if (square.getY > myPosition.getY) for (y <- myPosition.getY to square.getY) board(y)(square.getX).accessible = false
      myPosition = square
      myPosition.accessible = false
    }


    def possibleSilence(lastMove: String) = {
      (directions - oppositeDirection(lastMove)).flatMap(dir => takeSilenceSquares((myPosition, List.empty[(String, Square)]), dir, _.accessible)).map { sqpth =>
        val square = sqpth._1
        val direction = if (square.getX < myPosition.getX) ("W", myPosition.getX - square.getX, square) else
                        if (square.getX > myPosition.getX) ("E", square.getX - myPosition.getX, square) else
                        if (square.getY < myPosition.getY) ("N", myPosition.getY - square.getY, square) else
                        if (square.getY < myPosition.getY) ("S", square.getY - myPosition.getY, square) else (if (lastMove.isEmpty) "N" else lastMove, 0, myPosition)
        direction
      }
    }

    def possibleDirection = {
      val candidates = Array((myPosition.getX + 1, myPosition.getY, "E"), (myPosition.getX - 1, myPosition.getY, "W"), (myPosition.getX, myPosition.getY + 1, "S"), (myPosition.getX, myPosition.getY - 1, "N"))
      candidates.withFilter(xy => xy._1 >= 0 && xy._1 < 15 &&
                              xy._2 >= 0 && xy._2 < 15 &&
                              board(xy._2)(xy._1).accessible)
        .map(data => (board(data._2)(data._1), data._3))
    }
  }


//------------------------------------------FILE ENTRY------------------------------------------------------------------
/*
  val filename = "ocean/ocean2.txt"
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

  val myManager = new MySquareManager(board, legalSquares, flattenBoard, euclideanDistanceMap)
  val oppManager = new OppSquareManager(board, legalSquares, flattenBoard)
  val oppLegalSquaresMap = legalSquares.map((_, List.empty[PathInfo])).toMap
  var oppSquares = oppLegalSquaresMap
  var mySquares = oppLegalSquaresMap

  var needSilence = false
  var needDopCommand = true
  var oppPrevLife = 6
  var myLastMove = ""
  var myPrevLife = 6
  var torpedoOppRun = false
//  var torpedoMyRun = false
  var torpedoOppCoord = myManager.myPosition
  var torpedoMyCoord = (-1,-1)

//  board.foreach(bl => {
//    bl.foreach(_.print)
//     Console.err.println})

  println(s"${myManager.myPosition.getX} ${myManager.myPosition.getY}")
//  println("0 6")

  def processOppMove(oppSquares: OppPath, oppOrder: String, myLife: Int) = {
      val order = oppOrder.trim
//       Console.err.println(s"ORDER: $order")
      if (order.startsWith("MOVE")) oppManager.processOpponentMove(order, oppSquares)
      else if (order.startsWith("SURFACE")) oppManager.processOpponentSurface(order, oppSquares)
      else if (order.startsWith("SILENCE")) oppManager.processOpponentSilence(oppSquares)
      else if (order.startsWith("TORPEDO")) {
        if (myLife < myPrevLife) needSilence = true
//        Console.err.println(s"!!torpedoMyRun!!")
//        torpedoMyRun = true
        oppSquares
      }
      else if (order.startsWith("NA")) oppLegalSquaresMap
      else oppSquares
  }

  def processOppMoves(oppSquares: OppPath, opponentOrders: Array[String], myLife: Int) = {
    var opps = oppSquares
    opponentOrders.foreach { order =>
      opps = processOppMove(opps, order, myLife)
    }
    opps
  }


  def calculateSquares(oppSquares: OppPath, life: Int, prevLife: Int, opponentOrdersArr: Array[String], myLife: Int) = {
    val sqpth = if (torpedoOppRun) {
      torpedoOppRun = false
//      Console.err.println(s"torpedoRun")
      if (life < prevLife) {
        //        Console.err.println(s"torpedoCoord=${torpedoCoord} oppLife=$oppLife oppPrevLife=$oppPrevLife")
        if (prevLife - life == 2) {
          val oppsq = oppSquares.filter(sqpth => sqpth._1.index == torpedoOppCoord.index)
          //          Console.err.println(s"torpedo 2")
          //          oppsq.keys.toList.sortBy(_.index).foreach(Console.err.print)
          //          Console.err.println
          oppsq
        } else if (prevLife - life == 1) {
          //          Console.err.println(s"torpedo 1")
          val squares = oppManager.nextRawSquare(torpedoOppCoord)
          val oppsq = oppSquares.filter(sqpth => squares.contains(sqpth._1))
          //          oppsq.keys.toList.sortBy(_.index).foreach(Console.err.print)
          //          Console.err.println
          oppsq
        } else {
          Console.err.println(s"Double hit!") // FIXME
          oppSquares
        }
      } else {
        val squares = oppManager.nextRawSquare(torpedoOppCoord) + torpedoOppCoord
        val oppsq = oppSquares.filterNot(sqpth => squares.contains(sqpth._1))
        //          Console.err.println(s"Loose. New size= ${oppsq.size}")
        //          oppsq.keys.toList.sortBy(_.index).foreach(Console.err.print)    ,.
        //          Console.err.println1
        oppsq
      }
    } else oppSquares
    processOppMoves(sqpth, opponentOrdersArr, myLife)
  }

  def calculateLoad(torpedoCooldown: Int, silenceCooldown: Int, sonarCooldown: Int, mineCooldown: Int) = {
    if (needSilence) Console.err.println(s"need silence!")

    if (needSilence && silenceCooldown > 0) " SILENCE" else
    if (torpedoCooldown > 0) " TORPEDO" else
    if (mineCooldown > 0) " MINE" else
    if (silenceCooldown > 0) " SILENCE" else
    if (sonarCooldown > 0) " SONAR" else ""
  }

  def calculateMySilence(silenceCooldown: Int) = {
    if (!needSilence || silenceCooldown > 0) "" else {
      val possibleSilence = myManager.possibleSilence(myLastMove)

      Console.err.println(s"Possible silence: ")
      possibleSilence.foreach(s => Console.err.print(s"${s._1}${s._2} "))

      val mySilence = possibleSilence.find(_ => true).get
      needSilence = false
      myLastMove = mySilence._1
      s"|SILENCE ${mySilence._1} ${mySilence._2}"
    }
  }

  def calculateMyMove(directions: Array[(Square, String)], oppsq: Set[Square]) = {
    val dir = directions.map(sqch => {
      val minOpp = oppsq.minBy(s => euclideanDistanceMap((s, sqch._1)))
      (euclideanDistanceMap((minOpp, sqch._1)), sqch._1, sqch._2)
    }).minBy(_._1)
    // Console.err.println(s"$dir")
    //      val newpos = directions(myManager.rand.nextInt(directions.size))
    myLastMove = dir._3
    myManager.myPosition = dir._2
    s"MOVE ${dir._3}"
  }

  def calculateMyTorpedo(torpedoCooldown: Int, oppsq: Set[Square], myLife: Int, oppLife: Int) = {
    if (torpedoCooldown > 0 || oppsq.size > 150) "" else {
      val unsafeTorpedoSquares = myManager.unsafeTorpedoSquareMap(myManager.myPosition)
      val safeTorpedoSquares = myManager.safeTorpedoSquareMap(myManager.myPosition)
      val torpedoSquares = if (oppsq.size == 1 && myLife >= oppLife) unsafeTorpedoSquares else safeTorpedoSquares
      val nearOppCandidateCoord = torpedoSquares.intersect(oppsq)
      nearOppCandidateCoord.find(_ => true) match {
        case Some(s) => {
          torpedoOppRun = true
          oppPrevLife = oppLife
          torpedoOppCoord = s
          s"|TORPEDO ${s.getX} ${s.getY}"
        }
        case None => ""
      }
    }
  }

  def calculateMySurface(directions: Array[(Square, String)]) = {
    if (directions.isEmpty) {
      legalSquares.foreach(_.accessible = true)
      myManager.myPosition.accessible = false
      "SURFACE|"
    } else ""
  }

  def calculateMyMine(mineCooldown: Int, oppsq: Set[Square], myLife: Int, oppLife: Int) = {
    if (mineCooldown > 0) "" else {
      ""
    }
  }

  def calculate(oppsq: Set[Square],
                myLife: Int, oppLife: Int,
                torpedoCooldown: Int, silenceCooldown: Int, sonarCooldown: Int, mineCooldown: Int) = {
//    if (oppsq.size < 30) oppsq.toList.sortBy(_.index).foreach(Console.err.print)
    Console.err.println(s"oppsq.size=${oppsq.size}")

    val surface = calculateMySurface(myManager.possibleDirection)
    val move = calculateMyMove(myManager.possibleDirection, oppsq)
    val torpedo = calculateMyTorpedo(torpedoCooldown, oppsq, myLife, oppLife)
    val mine = calculateMyMine(mineCooldown, oppsq, myLife, oppLife)
    val silence = calculateMySilence(silenceCooldown)
    val load = calculateLoad(torpedoCooldown, silenceCooldown, sonarCooldown, mineCooldown)
    surface + move + load + torpedo + silence
  }

  while (true) {
    val Array(x, y, myLife, oppLife, torpedoCooldown, sonarCooldown, silenceCooldown, mineCooldown) = (readLine split " ").map(_.toInt)
    Console.err.println(s"sonarCooldown=$sonarCooldown")
    myManager.setMyPosition(board(y)(x))
    // Console.err.println(s"$x $y $myLife $oppLife $torpedoCooldown $sonarCooldown $silenceCooldown $mineCooldown")
    val sonarResult = readLine
    // Console.err.println(s"sonarResult=$sonarResult")
    val opponentOrders = readLine
    // Console.err.println(s"opponentOrders=$opponentOrders")
    val opponentOrdersArr = opponentOrders.split("\\|")
//    Console.err.println(s"myLife=$myLife myPrevLife=$myPrevLife needSilence=$needSilence")

    oppSquares = calculateSquares(oppSquares, oppLife, oppPrevLife, opponentOrdersArr, myLife)

    val output = calculate(oppSquares.keySet, myLife, oppLife, torpedoCooldown, silenceCooldown, sonarCooldown, mineCooldown)

    myPrevLife = myLife

    println(output)
  }
}