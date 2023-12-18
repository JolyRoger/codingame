package codingames.easy.triangletoggle

import math._
import scala.io.Source
import scala.util._
import scala.io.StdIn._

object Solution extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
  val filename = "resources/triangle/condensed.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines
  def readInt = if (data.hasNext) data.next.toInt else {System.exit(0); -1}
  def readLine = if (data.hasNext) data.next else {System.exit(0); ""}
// ---------------------------------------------------------------------------------------------------------------------
//  case class Point(x: Int, y: Int)
  type Point = (Double, Double)
  case class Triangle(p1: Point, p2: Point, p3: Point)

  val Array(hi, wi) = (readLine split " ").filter(_ != "").map (_.toInt)
  Console.err.println(s"$hi $wi")
  val styleSym = if (readLine equals "expanded") " " else ""
  Console.err.println(s"$styleSym")
  val howManyTriangles = readLine.toInt
  Console.err.println(s"$howManyTriangles")

  val triangleList = for(_ <- 0 until howManyTriangles) yield {
    val Array(x1, y1, x2, y2, x3, y3) = (readLine split " ").filter(_ != "").map (_.toInt)
    Console.err.println(s"$x1, $y1, $x2, $y2, $x3, $y3")
    Triangle((x1.toDouble, y1.toDouble), (x2.toDouble, y2.toDouble), (x3.toDouble, y3.toDouble))
//    Triangle(Point(x1, y1), Point(x2, y2), Point(x3, y3))
  }

//  def sign(p1: Point, p2: Point, p3: Point) = (p1.x - p3.x) * (p2.y - p3.y) - (p2.x - p3.x) * (p1.y - p3.y);
//
//  def pointInTriangle(pt: Point, v1: Point, v2: Point, v3: Point) = {
//    val d1 = sign(pt, v1, v2);
//    val d2 = sign(pt, v2, v3);
//    val d3 = sign(pt, v3, v1);
//
//    val has_neg = (d1 < 0) || (d2 < 0) || (d3 < 0);
//    val has_pos = (d1 > 0) || (d2 > 0) || (d3 > 0);
//
//    !(has_neg && has_pos);
//  }

  def isPointInsideTriangle(point: Point, vertex1: Point, vertex2: Point, vertex3: Point): Boolean = {
    val (x, y) = point
    val (x1, y1) = vertex1
    val (x2, y2) = vertex2
    val (x3, y3) = vertex3

    // Calculate barycentric coordinates
    val detT = (y2 - y3) * (x1 - x3) + (x3 - x2) * (y1 - y3)
    val alpha = ((y2 - y3) * (x - x3) + (x3 - x2) * (y - y3)) / detT
    val beta = ((y3 - y1) * (x - x3) + (x1 - x3) * (y - y3)) / detT
    val gamma = 1 - alpha - beta

    // Check if the point is inside the triangle
    alpha >= 0 && beta >= 0 && gamma >= 0
  }

  val tr = Triangle((0d,0d), (0d,5d), (5d,0d))
  val p = (1d,1d)
  val res = isPointInsideTriangle(p, tr.p1, tr.p2, tr.p3)
  Console.err.println(s"Res=$res")

  for(y <- 0 until hi) {
    var str = " "
    for (x <- 0 until wi) {
//      val isSpace = triangleList.exists(triangle => pointInTriangle(Point(x, y), triangle.p1, triangle.p2, triangle.p3))
      val numberInPoint = triangleList.count(triangle => isPointInsideTriangle((x.toDouble, y.toDouble), triangle.p1, triangle.p2, triangle.p3))
      val isSpace = numberInPoint % 2 == 1
      str = if (isSpace) str + " " + styleSym else  str + "*" + styleSym
    }
    println(str.trim)
  }
}
