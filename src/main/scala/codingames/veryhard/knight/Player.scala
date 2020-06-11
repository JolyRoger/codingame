package codingames.veryhard.knight

import math._
import scala.io.StdIn._
import scala.reflect.ClassTag

object Player extends App {
  type Point = (Int, Int)
  type Matrix[T] = Array[T]

  val Array(w, h) = for (i <- readLine split " ") yield i.toInt
  Console.err.println(s"w=$w h=$h")
  val n = readInt // maximum number of turns before game over.
  Console.err.println(s"n=$n")
  val Array(x0, y0) = for (i <- readLine split " ") yield i.toInt
  Console.err.println(s"x0=$x0 y0=$y0")

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

  var prevMatrix = distance((x, y), dimension)

  def nextMove(x: Int, y: Int, bombdir: String): (Int, Int) = {
    if (bombdir == "UNKNOWN") {
      val uncheckedPoint = cutMatrix.zipWithIndex.filter(_._1 == true)
      val semisize = uncheckedPoint.length / 2
      val index = toMatrix(uncheckedPoint(semisize)._2)
      cutMatrix(index) = false
      index
    } else {
      var distanceMatrix = distance((x, y), dimension)
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