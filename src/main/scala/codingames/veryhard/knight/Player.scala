package codingames.veryhard.knight

import math._
import scala.io.Source
import scala.io.StdIn._
import scala.reflect.ClassTag

object Player extends App {
  type Point = (Int, Int)
  type Matrix[T] = Array[Array[T]]
  type DistData = Matrix[Matrix[Double]]

  //------------------------------------------FILE ENTRY------------------------------------------------------------------
        val filename = "resources/knight.txt"
        val bufferedSource = Source.fromFile(filename)
        val data = bufferedSource.getLines
        def readInt = if (data.hasNext) data.next.toInt else -1
        def readLine = if (data.hasNext) data.next else "EOF"
  //----------------------------------------------------------------------------------------------------------------------

  var euclideanCounter = 0
  val Array(w, h) = for (i <- readLine split " ") yield i.toInt
  Console.err.println(s"w[idth]=$w\th[eight]=$h")
  val n = readInt // maximum number of turns before game over.
  Console.err.println(s"n[maximum number of turns before game over]=$n")
  val Array(x0, y0) = for (i <- readLine split " ") yield i.toInt
  Console.err.println(s"x0[initial]=$x0\ty0[initial]=$y0")

  val size = w * h
  val distances: Array[Array[Double]] = Array.fill[Array[Double]](size)(new Array(size))
  val distanceData: DistData = Array.ofDim[Double](w,h,w,h)

  for (p1y <- 0 until h;p1x <- 0 until w; p2y <- 0 until h;p2x <- 0 until w) {                                         // FIXME
    if (distanceData(p1x)(p1y)(p2x)(p2y) < 0.5) {
      val distance = euclidean((p1x,p1y), (p2x,p2y))
      distanceData(p1x)(p1y)(p2x)(p2y) = distance
      distanceData(p2x)(p2y)(p1x)(p1y) = distance
      if (p1y < w && p2y < w && p1x < h && p2x < h) {
        distanceData(p1y)(p1x)(p2y)(p2x) = distance
        distanceData(p2y)(p2x)(p1y)(p1x) = distance
      }
    }
  }

  Console.err.println(s"euclideanCounter=$euclideanCounter")
//  printData(w, distanceData)

  val dimension = (w, h)
  var x = x0
  var y = y0
  var cutMatrix = Array.fill[Boolean](w * h)(true)
  cutMatrix((x, y)) = false

  val doubleMapFunction = (unit: (Double, Double)) => unit._1 - unit._2
  val booleanMapFunction = (unit: (Boolean, Boolean)) => unit._1 && unit._2

  def printData(w: Int, dist: DistData) = {
    for (i <- 0 until size) {
      Console.err.println(s"======================")
      val p1 = toMatrix(w, i)
      printMatrix(dist(p1._1)(p1._2))
    }
  }
  def printMatrix(matrix: Matrix[Double]) = {
    for (j <- matrix(0).indices; i <- matrix.indices) {
      val point = matrix(i)(j)
      Console.err.print(s"${if (i == 0) "\n" else "\t"}")
      Console.err.print(s"[$i,$j]=")
      Console.err.print(f"$point%1.2f")
    }
    Console.err.println
  }

  def ~=(x: Double, y: Double, precision: Double) = (x - y).abs < precision
  def toMatrix(w : Int, number: Int): (Int, Int) = (number % w, number / w)
  implicit def toNumber(point: (Int, Int)): Int = point._2 * w + point._1 % w
  def euclidean(a: Point, b: Point) = {
    euclideanCounter += 1
    sqrt(pow(b._1 - a._1, 2) + pow(b._2 - a._2, 2))
  }
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

//  var prevMatrix = distance((x, y), dimension)
  var prevMatrix = distances(y * w + x)

  def nextMove(x: Int, y: Int, bombdir: String): (Int, Int) = {
    if (bombdir == "UNKNOWN") {
      val uncheckedPoint = cutMatrix.zipWithIndex.filter(_._1)
      val semisize = uncheckedPoint.length / 2
      val index = toMatrix(w, uncheckedPoint(semisize)._2)
      cutMatrix(index) = false
      index
    } else {
//      var distanceMatrix = distance((x, y), dimension)
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