package codingame.veryhard.knight

import math._
import scala.util._

/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/
object Player extends App {
  type Point = (Int, Int)
  type Matrix[T] = Array[T]
  var step = 0
//  val target = (4, 1)
//  val target = (10, 23)
//  val target = (10, 4)
  val target = (5, 0)
  Console.err.println(s"target=$target")
  // w: width of the building.
  // h: height of the building.
//  val Array(w, h) = for (i <- readLine split " ") yield i.toInt
//  val (w, h) = (5, 3)
//  val (w, h) = (18, 32)
//  val (w, h) = (16, 5)
  val (w, h) = (50, 50)
  Console.err.println(s"w=$w h=$h")
//  val n = readInt // maximum number of turns before game over.
  val n = 16 // maximum number of turns before game over.
  Console.err.println(s"n=$n")
//  val Array(x0, y0) = for (i <- readLine split " ") yield i.toInt
//  val (x0, y0) = (1, 1)
  val (x0, y0) = (17, 29)
//  val (x0, y0) = (5, 1)
  Console.err.println(s"x0=$x0 y0=$y0")

  val dimension = (w, h)
  var x = x0
  var y = y0
  var cutMatrix = Array.fill[Boolean](w * h)(true)
  cutMatrix((x, y)) = false

  def printMatrix[T](dimension: Point, matrix: Array[T]) = {
    for (j <- 0 until dimension._2; i <- 0 until dimension._1) {
      matrix(toNumber((i, j))) match {
        case point: Double => {
          Console.err.print(s"${if (i == 0) "\n" else "\t"}")
          Console.err.print(f"$point%1.2f")
        }
        case point: Boolean => {
          Console.err.print(s"${if (i == 0) s"\n$i:\t" else ""}")
          Console.err.print(s"${if (point) "*" else "+"}")
        }
      }
    }
    Console.err.println
  }

  def ~=(x: Double, y: Double, precision: Double) = (x - y).abs < precision
  def toMatrix(number: Int): (Int, Int) = (number % w, number / w)
  implicit def toNumber(point: (Int, Int)): Int = point._2 * w + point._1 % w
  val doubleMapFunction = (unit: (Double, Double)) => unit._1 - unit._2
  val booleanMapFunction = (unit: (Boolean, Boolean)) => unit._1 && unit._2
  def euclidean(a: Point, b: Point) = sqrt(pow(b._1 - a._1, 2) + pow(b._2 - a._2, 2))
  def distance(from: Point, dimension: Point) = (for (j <- 0 until dimension._2; i <- 0 until dimension._1) yield euclidean(from, (i, j))).toArray
  def delta[T](matrix1: Array[T], matrix2: Array[T], mapFunction: ((T, T)) => T) = matrix1 zip matrix2 map mapFunction
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

  var prevMatrix = distance((x, y), dimension)

  def nextMove(x: Int, y: Int, bombdir: String): (Int, Int) = {
    if (bombdir == "UNKNOWN") {
      Console.err.println(s"unknown section")
      Console.err.println(s"$x,$y $bombdir")
      Console.err.print(s"PREV MATRIX:")
      printMatrix(dimension, prevMatrix)
      Console.err.print(s"CUT MATRIX:")
      printMatrix(dimension, cutMatrix)
      val uncheckedPoint = cutMatrix.zipWithIndex.filter(_._1 == true)
      val semisize = uncheckedPoint.length / 2
      val index = toMatrix(uncheckedPoint(semisize)._2)
      Console.err.println(s"Jump to $index")
      cutMatrix(index) = false
      Console.err.print(s"CUT MATRIX:")
      printMatrix(dimension, cutMatrix)
      index
    } else {
      Console.err.println(s"state section ->")
      var distanceMatrix = distance((x, y), dimension)
      Console.err.print(s"PREV")
      printMatrix(dimension, prevMatrix)
      Console.err.print(s"MATRIX")
      printMatrix(dimension, distanceMatrix)
      val processedMatrix = Player.delta[Double](prevMatrix, distanceMatrix, doubleMapFunction).toArray
      Console.err.print(s"PROCESSED MATRIX")
      printMatrix(dimension, processedMatrix)
      Console.err.print(s"CUT MATRIX:")
      printMatrix(dimension, cutMatrix)
      val cutPoints = cut(processedMatrix, cutMatrix, bombdir, booleanMapFunction).toArray
      Console.err.print(s"NEW CUT MATRIX")
      printMatrix(dimension, cutPoints)
      prevMatrix = distanceMatrix
      cutMatrix = cutPoints
      nextMove(x, y, "UNKNOWN")
    }
  }

  var stopFlag = false
  var prevDistanceToTarget = euclidean((x0, y0), target)
  Console.err.println(s"prevDistanceToTarget=$prevDistanceToTarget")

  def calcBombdirs(x: Int, y: Int) = {
    val distance = euclidean((x, y), target)
    val state = if (x == x0 && y == y0) "UNKNOWN" else {
      if (~=(distance, prevDistanceToTarget, 0.001)) "SAME" else
      if (distance > prevDistanceToTarget) "COLDER" else "WARMER"
    }
    prevDistanceToTarget = distance
    state
  }

  // game loop
  while (!stopFlag) {
//    val bombdir = readLine // Current distance to the bomb compared to previous distance (COLDER, WARMER, SAME or UNKNOWN)
    Console.err.println(s"\nstep=$step")
    val bombdir = calcBombdirs(x, y)
    Console.err.println(s"bombdir=$bombdir")

    val (x_, y_) = nextMove(x, y, bombdir)
    x = x_
    y = y_

    if (y == target._2 && x == target._1) {
      Console.err.println(s"SUCCESS! step $step of $n")
      println(s"Target is $x $y")
      stopFlag = true
    } else if (step >= n) {
      stopFlag = true
      Console.err.println(s"FAIL :(")
    }
    step = step + 1
  }
}
