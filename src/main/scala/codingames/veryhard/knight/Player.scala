package codingames.veryhard.knight

import math._
import scala.io.StdIn._
import scala.reflect.ClassTag

object Player extends App {
  type Point = (Int, Int)
  type Matrix[T] = Array[T]

  var euclideanCounter = 0
  val Array(w, h) = for (i <- readLine split " ") yield i.toInt
  val n = readInt // maximum number of turns before game over.
  val Array(x0, y0) = for (i <- readLine split " ") yield i.toInt

  val size = w * h
  val distances: Array[Array[Double]] = Array.ofDim[Double](size,size)

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

  val doubleMapFunction = (unit: (Double, Double)) => unit._1 - unit._2
  val booleanMapFunction = (unit: (Boolean, Boolean)) => unit._1 && unit._2

  def ~=(x: Double, y: Double, precision: Double) = (x - y).abs < precision
  def toMatrix(number: Int): (Int, Int) = (number % w, number / w)
  implicit def toNumber(point: (Int, Int)): Int = point._2 * w + point._1 % w
  def euclidean(a: Point, b: Point) = sqrt(pow(b._1 - a._1, 2) + pow(b._2 - a._2, 2))
  def distance(from: Point, dimension: Point) = (for (j <- 0 until dimension._2; i <- 0 until dimension._1) yield euclidean(from, (i, j))).toArray
  def delta[T:ClassTag](matrix1: Array[T], matrix2: Array[T], mapFunction: ((T, T)) => T) = matrix1 zip matrix2 map mapFunction
  def cut(resMatrix: Array[Double], cutMatrix: Array[Boolean], bombdir: String,
          bmp: ((Boolean, Boolean)) => Boolean) = {
    val newCut = resMatrix.map { bombdir match {
      case "SAME" => ~=(_, 0, 0.001)
      case "COLDER" => _ < 0
      case "WARMER" => _ > 0
    }
    }
    delta[Boolean](cutMatrix, newCut, bmp)
  }

  var prevMatrix = distances(y * w + x)

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

  def nextMove(x: Int, y: Int, bombdir: String): (Int, Int) = {
    if (bombdir == "UNKNOWN") {
      val uncheckedPoint = cutMatrix.zipWithIndex.filter(_._1 == true).map(_._2)
      val index = findPoint(uncheckedPoint, cutMatrix)
      cutMatrix(index) = false
      index
    } else {
      val distanceMatrix = distances(y * w + x)
      val processedMatrix = Player.delta[Double](prevMatrix, distanceMatrix, doubleMapFunction).toArray
      val cutPoints = cut(processedMatrix, cutMatrix, bombdir, booleanMapFunction).toArray
      prevMatrix = distanceMatrix
      cutMatrix = cutPoints
      nextMove(x, y, "UNKNOWN")
    }
  }

  // game loop
  while (true) {
    val bombdir = readLine // Current distance to the bomb compared to previous distance (COLDER, WARMER, SAME or UNKNOWN)
    val (x_, y_) = nextMove(x, y, bombdir)
    x = x_
    y = y_

    println(s"$x $y")
  }
}