package codingames.challenge.ocean

import math._
import scala.io.Source
import scala.util._
import scala.io.StdIn._

class Square(x: Int, y: Int, sym: Char) {
  var leftMoveNum: Int = -1
  var rightMoveNum: Int = -1
  var upMoveNum: Int = -1
  var downMoveNum: Int = -1

  var leftMoveNumInit: Int = -1
  var rightMoveNumInit: Int = -1
  var upMoveNumInit: Int = -1
  var downMoveNumInit: Int = -1

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
  override def toString = s"($x,$y)$sym"
}

class EnemySquareManager(legalSquares: Array[Square]) {

  def processOpponentMove(opponentOrders: String) = {
    val Array(_, direction) = opponentOrders.split("\\s")
    direction match {
      case "N" => legalSquares.filter(square => { square.upMoveNum -= 1; square.upMoveNum > -1 })
      case "S" => legalSquares.filter(square => { square.downMoveNum -= 1; square.downMoveNum > -1 })
      case "W" => legalSquares.filter(square => { square.leftMoveNum -= 1; square.leftMoveNum > -1 })
      case "E" => legalSquares.filter(square => { square.rightMoveNum -= 1; square.rightMoveNum > -1 })
    }
  }

  def processOpponentSurface(opponentOrders: String) = {
    val Array(_, sector) = opponentOrders.split("\\s")

    legalSquares.foreach(square => {
      square.leftMoveNum = square.leftMoveNumInit
      square.rightMoveNum = square.rightMoveNumInit
      square.upMoveNum = square.upMoveNumInit
      square.downMoveNum = square.downMoveNumInit
    })

    sector match {
      case "1" =>
      case "2" =>
      case "3" =>
      case "4" =>
      case "5" =>
      case "6" =>
      case "7" =>
      case "8" =>
      case "9" =>
    }
  }
}

class MySquareManager(board: Array[Array[Square]]) {
  val flattenBoard = board.flatten
  val coordSquaresMap = flattenBoard.map(square => ((square.getX, square.getY), square)).toMap
  val legalSquares = flattenBoard.filter(_.accessible)
  val rand = new Random(System.currentTimeMillis)
//  var myPosition = coordSquaresMap((0, 8))
  var myPosition = legalSquares(rand.nextInt(legalSquares.length))
  Console.err.println(s"my position=$myPosition")

  val torpedoSquareMap = legalSquares.map(square => (square, square.allTorpedoSquares.filter(coordSquaresMap(_).water))).toMap
  val safeTorpedoSquareMap = torpedoSquareMap.map(kv => (kv._1, kv._2.filter { xy =>
    (Math.abs(kv._1.getX - xy._1) > 1 || Math.abs(kv._1.getY - xy._2) > 1) && coordSquaresMap(xy).water
  }))

  setSearchIndices

//  legalSquares.foreach(square => Console.err.println(s"$square : ${square.leftMoveNum} ${square.upMoveNum} ${square.rightMoveNum} ${square.downMoveNum}"))
  legalSquares.foreach(square => {
    square.leftMoveNumInit = square.leftMoveNum
    square.rightMoveNumInit = square.rightMoveNum
    square.upMoveNumInit = square.upMoveNum
    square.downMoveNumInit = square.downMoveNum
  })

  myPosition.accessible = false


  private def setSearchIndices {
    for (y <- board.indices) {
      var indexHor = -1
      var indexVer = -1
      for (x <- board(y).indices) {
        val squareHor = board(y)(x)
        val squareVer = board(x)(y)
        indexHor = if (squareHor.water) indexHor + 1 else -1
        indexVer = if (squareVer.water) indexVer + 1 else -1
        squareHor.leftMoveNum = indexHor
        squareVer.upMoveNum = indexVer
      }

      indexHor = -1
      indexVer = -1

      for (x <- board(y).indices.reverse) {
        val squareHor = board(y)(x)
        val squareVer = board(x)(y)
        indexHor = if (squareHor.water) indexHor + 1 else -1
        indexVer = if (squareVer.water) indexVer + 1 else -1
        squareHor.rightMoveNum = indexHor
        squareVer.downMoveNum = indexVer
      }
    }
  }

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
    candidates.filter(xy => xy._1 >= 0 && xy._1 < 15 &&
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
  val myManager = new MySquareManager(board)
  val oppManager = new EnemySquareManager(myManager.legalSquares)

  board.foreach(bl => {
    bl.foreach(_.print)
    Console.err.println})

  println(s"${myManager.myPosition.getX} ${myManager.myPosition.getY}")


  while (true) {
    val Array(x, y, myLife, oppLife, torpedoCooldown, sonarCooldown, silenceCooldown, mineCooldown) = (readLine split " ").map(_.toInt)
    myManager.setMyPosition(board(y)(x))
    Console.err.println(s"$x $y $myLife $oppLife $torpedoCooldown $sonarCooldown $silenceCooldown $mineCooldown")
    val sonarResult = readLine
    Console.err.println(s"sonarResult=$sonarResult")
    val opponentOrders = readLine
    Console.err.println(s"opponentOrders=$opponentOrders")
    if (opponentOrders.startsWith("MOVE")) oppManager.processOpponentMove(opponentOrders)
    else if (opponentOrders.startsWith("SURFACE")) oppManager.processOpponentSurface(opponentOrders)

    val directions = myManager.possibleDirection
    val mainCommand = if (directions.isEmpty) "SURFACE" else s"MOVE ${directions(myManager.rand.nextInt(directions.length))} TORPEDO"

    val dopCommand = if (mainCommand == "SURFACE") {
      myManager.surface
    } else if (mainCommand.startsWith("MOVE")) {
      s"${if (torpedoCooldown > 0) "" else {
        val torpedoSquares = myManager.safeTorpedoSquares
        torpedoSquares.find(_ => true) match {
          case Some(s) => {
            s"|TORPEDO ${s._1} ${s._2}"
          }
          case None => ""
        }
      }}"
    }

    println(s"$mainCommand$dopCommand")
  }
}