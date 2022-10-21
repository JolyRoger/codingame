package codingames.veryhard.knight

import math._
import scala.io.Source
import scala.io.StdIn._
import scala.reflect.ClassTag

object Player0 extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
  val filename = "resources/knight/lot-of-jumps.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines
  def readInt = if (data.hasNext) data.next.toInt else { System.exit(0); -1 }
  def readLine = if (data.hasNext) data.next else { System.exit(0); "" }
//----------------------------------------------------------------------------------------------------------------------
  type Point = (Int, Int)
  type Matrix[T] = Array[T]

  var euclideanCounter = 0
  val Array(w, h) = for (i <- readLine split " ") yield i.toInt
  Console.err.println(s"$w $h")
  val n = readInt // maximum number of turns before game over.
  Console.err.println(n)
  val Array(x0, y0) = for (i <- readLine split " ") yield i.toInt
  Console.err.println(s"${x0} ${y0}")

  val size = w * h
  val distances: Array[Array[Float]] = Array.ofDim[Float](size,size)

  for (p1y <- 0 until h;p1x <- 0 until w; p2y <- 0 until h;p2x <- 0 until w) {                                         // FIXME
    val p1Index = p1y * w + p1x
    val p2Index = p2y * w + p2x
    if (distances(p1Index)(p2Index) < 0.5) {
      val distance = euclidean((p1x,p1y), (p2x,p2y))
      distances(p1Index)(p2Index) = distance
      distances(p2Index)(p1Index) = distance
    }
  }

  val dimension = (w, h)
  var x = x0
  var y = y0
  var cutMatrix = Array.fill[Boolean](w * h)(true)
  cutMatrix((x, y)) = false

  val floatMapFunction = (unit: (Float, Float)) => unit._1 - unit._2
  val booleanMapFunction = (unit: (Boolean, Boolean)) => unit._1 && unit._2

  def ~=(x: Float, y: Float, precision: Float) = (x - y).abs < precision
  def toMatrix(number: Int): (Int, Int) = (number % w, number / w)
  implicit def toNumber(point: (Int, Int)): Int = point._2 * w + point._1 % w
  def euclidean(a: Point, b: Point): Float = sqrt(pow(b._1 - a._1, 2) + pow(b._2 - a._2, 2)).toFloat
  def distance(from: Point, dimension: Point) = (for (j <- 0 until dimension._2; i <- 0 until dimension._1) yield euclidean(from, (i, j))).toArray
  def delta[T:ClassTag](matrix1: Array[T], matrix2: Array[T], mapFunction: ((T, T)) => T) = matrix1 zip matrix2 map mapFunction
  def cut(resMatrix: Array[Float], cutMatrix: Array[Boolean], bombdir: String,
          bmp: ((Boolean, Boolean)) => Boolean) = {
    val newCut = resMatrix.map { bombdir match {
      case "SAME" => ~=(_, 0, 0.001f)
      case "COLDER" => _ < 0
      case "WARMER" => _ > 0
    }
    }
    delta[Boolean](cutMatrix, newCut, bmp)
  }

  var prevMatrix = distances(y * w + x)

  def printMatrix[T](dimension: Point, matrix: Array[T]) = {
    for (j <- 0 until dimension._2;i <- 0 until dimension._1) {
      Console.err.print(s"${if (i == 0) "\n" else ""}")
      matrix(toNumber((i, j))) match {
        case point: Float => Console.err.print(f"$point%1.2f")
        case point: Boolean => Console.err.print(s"${if (point) '*' else '#'}")
      }
    }
    Console.err.println
  }

  def findPoint(uncheckedPoint: Array[Int], cutMatrix: Array[Boolean]): (Int, Int) = {
    val mpoints = uncheckedPoint.map(point => toMatrix(point))
    val maxXIndex = mpoints.maxBy(_._1)._1
    val minXIndex = mpoints.minBy(_._1)._1
    val maxYIndex = mpoints.maxBy(_._2)._2
    val minYIndex = mpoints.minBy(_._2)._2
    val mediX = minXIndex + Math.round(maxXIndex - minXIndex) / 2
    val mediY = minYIndex + Math.round(maxYIndex - minYIndex) / 2
    var counter = 0
    var point = (-1,-1)

    while (point._1 == -1) {
      if (cutMatrix((mediX, mediY))) {
        point = (mediX, mediY)
      } else if (cutMatrix((mediX + counter, mediY))) {
        point = (mediX + counter, mediY)
      } else if (cutMatrix((mediX, mediY + counter))) {
        point = (mediX, mediY + counter)
      } else if (cutMatrix((mediX - counter, mediY))) {
        point = (mediX - counter, mediY)
      } else if (cutMatrix((mediX, mediY - counter))) {
        point = (mediX, mediY - counter)
      } else if (cutMatrix((mediX + counter, mediY - counter))) {
        point = (mediX + counter, mediY - counter)
      } else if (cutMatrix((mediX - counter, mediY + counter))) {
        point = (mediX - counter, mediY + counter)
      } else if (cutMatrix((mediX + counter, mediY + counter))) {
        point = (mediX + counter, mediY + counter)
      } else if (cutMatrix((mediX - counter, mediY - counter))) {
        point = (mediX - counter, mediY - counter)
      } else counter += 1
    }
    point
  }

  def firstMove(x: Int, y: Int) = {
    val halfSize = size / 2
    val indexCandidate = toMatrix(halfSize)
    if (indexCandidate == (x, y)) (indexCandidate._1 - 1, indexCandidate._2) else indexCandidate
  }

  def nextMove(x: Int, y: Int, bombdir: String): (Int, Int) = {
    if (bombdir == "UNKNOWN") {
      val index = firstMove(x, y)
      cutMatrix(index) = false
      index
    } else {
      val distanceMatrix = distances(y * w + x)
      val processedMatrix = delta[Float](prevMatrix, distanceMatrix, floatMapFunction).toArray
      val cutPoints = cut(processedMatrix, cutMatrix, bombdir, booleanMapFunction).toArray
      prevMatrix = distanceMatrix
      cutMatrix = cutPoints
      // nextMove(x, y, "UNKNOWN")
      val uncheckedPoint = cutMatrix.zipWithIndex.withFilter(_._1).map(_._2)
      val index = findPoint(uncheckedPoint, cutMatrix)
      Console.err.println(s"${cutMatrix.count(!_)}")
      cutMatrix(index) = false
      index
    }
  }

  val res = for (i <- 0 until size) yield {
    val (x, y) = toMatrix(i)
    if (x == x0 && y == y0) (x, y, 0, 0)
    else {
      val distanceMatrix = distances(y * w + x)
      val processedMatrix = delta[Float](prevMatrix, distanceMatrix, floatMapFunction)
      val warmerPoints = cut(processedMatrix, cutMatrix, "WARMER", booleanMapFunction)
      val colderPoints = cut(processedMatrix, cutMatrix, "COLDER", booleanMapFunction)
      val samePoints = cut(processedMatrix, cutMatrix, "SAME", booleanMapFunction)
      val warmerPointsNum = warmerPoints.count(!_)
      val colderPointsNum = colderPoints.count(!_)
      val samePointsNum = samePoints.count(!_)
      Console.err.println(s"\t(x0,y0)=($x0,$y0) (x,y)=($x,$y) WARMER=$warmerPointsNum COLDER=$colderPointsNum SAME=$samePointsNum")
      (x, y, warmerPointsNum, colderPointsNum)
    }
  }
  val (xw, yw, maxWinW, maxCinW) = res.maxBy(_._3)
  val (xc, yc, maxWinC, maxCinC) = res.maxBy(_._4)

  Console.err.println(s"MAXWARMER: (x0,y0)=($x0,$y0) (x,y)=($xw,$yw) WARMER=$maxWinW COLDER=$maxCinW")
  Console.err.println(s"MAXCOLDER: (x0,y0)=($x0,$y0) (x,y)=($xc,$yc) WARMER=$maxWinC COLDER=$maxCinC")
//  res.foreach(data => {
//    val (x, y, warmerPointsNum, colderPointsNum) = data
//    Console.err.println(s"(x0,y0)=($x0,$y0) (x,y)=($x,$y) WARMER=$warmerPointsNum COLDER=$colderPointsNum")
//  })

  while (true) {
    val bombdir = readLine // Current distance to the bomb compared to previous distance (COLDER, WARMER, SAME or UNKNOWN)

    Console.err.println(bombdir)

    val (x_, y_) = nextMove(x, y, bombdir)
    x = x_
    y = y_

//    println(s"$x $y")
    // printMatrix(dimension, cutMatrix)
  }
}