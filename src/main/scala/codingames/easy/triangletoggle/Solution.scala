package codingames.easy.triangletoggle

import scala.io.Source

object Solution extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
  val filename = "resources/triangle/triangleInside.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines
  def readInt = if (data.hasNext) data.next.toInt else {System.exit(0); -1}
  def readLine = if (data.hasNext) data.next else {System.exit(0); ""}
// ---------------------------------------------------------------------------------------------------------------------
  case class Point(x: Int, y: Int)
  case class Triangle(p1: Point, p2: Point, p3: Point)

  val Array(hi, wi) = (readLine split " ").filter(_ != "").map (_.toInt)
  Console.err.println(s"$hi $wi")
  val styleStr = readLine
  val isExpanded = styleStr.equals("expanded")
  Console.err.println(s"$styleStr")
  val howManyTriangles = readLine.toInt
  Console.err.println(s"$howManyTriangles")

  val triangleList = for(_ <- 0 until howManyTriangles) yield {
    val Array(x1, y1, x2, y2, x3, y3) = (readLine split " ").filter(_ != "").map (_.toInt)
    Console.err.println(s"$x1, $y1, $x2, $y2, $x3, $y3")
    Triangle(Point(x1, y1), Point(x2, y2), Point(x3, y3))
  }

  def styleSym(x: Int) = if (isExpanded && x < wi - 1) " " else ""
  def sign(p1: Point, p2: Point, p3: Point) = (p1.x - p3.x) * (p2.y - p3.y) - (p2.x - p3.x) * (p1.y - p3.y);

  def pointInTriangle(pt: Point, v1: Point, v2: Point, v3: Point) = {
    val d1 = sign(pt, v1, v2);
    val d2 = sign(pt, v2, v3);
    val d3 = sign(pt, v3, v1);

    val has_neg = (d1 < 0) || (d2 < 0) || (d3 < 0);
    val has_pos = (d1 > 0) || (d2 > 0) || (d3 > 0);

    !(has_neg && has_pos);
  }

  for(y <- 0 until hi) {
    var str = ""
    for (x <- 0 until wi) {
      val numberInPoint = triangleList.count(triangle => pointInTriangle(Point(x, y), triangle.p1, triangle.p2, triangle.p3))
      val isSpace = numberInPoint % 2 == 1
      str = if (isSpace) str + " " + styleSym(x) else  str + "*" + styleSym(x)
    }
    println(str)
  }
}
