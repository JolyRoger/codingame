package codingame.puzzle.rooks.v2

import scala.collection.mutable
import scala.io.Source

object Solution extends App {
  val filename = "rooks2.txt"
  type Point = (Int, Int)
  type Rooks = Map[Point, Boolean]
  type Matrix = Array[Array[Boolean]]

  class Graph(horRect: List[Rect], verRect: List[Rect]) {
    val size = horRect.length + verRect.length
    val adj = (for (i <- 0 until size) yield Set[Int]()).toArray

    squareRectIndexMap.foreach(data => {
      addEdge(data._2.head, data._2(1))
    })

    def addEdge(v: Int, w: Int){
      adj(v) = adj(v) + w
      adj(w) = adj(w) + v
    }
  }

  class Rect(val data: List[Point], val index: Int)

  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines().toList

  data.foreach(c => Console.err.println(s"$c"))
  val (n, lines) = (data.head.toInt, data.tail)

  var squareRectIndexMap = mutable.Map.empty[Point, List[Int]]
  var rectIndexHorDataMap = mutable.Map.empty[Int, List[Point]]
  var rectIndexVerDataMap = mutable.Map.empty[Int, List[Point]]

  def linesToMatrix(lines: List[String]) = lines.map(_.toCharArray.map(_ == '.')).toArray

  def matrixToIndices(matrix: Array[Array[Boolean]]) = matrix.zipWithIndex.flatMap(rowIndex =>
    rowIndex._1.zipWithIndex.map(colIndex =>
      ((rowIndex._2, colIndex._2), colIndex._1))).toMap

  var rectIndex = -1

  def horMatrix(rooks: Rooks) = {

    def calculateRow(initRow: Int, initCol: Int) = {
      var row = initRow
      var col = initCol
      var out = List.empty[Point]

      while(col < n && rooks((row, col))) {
        out ::= (row, col)
        col += 1
      }
      rectIndex += 1
      (new Rect(out, rectIndex), col + 1)
    }

    var row = 0
    var col = 0
    var out = List.empty[Rect]

    while(row < n && col < n) {
      val point = (row, col)
      val (rect, newCol) = calculateRow(row, col)

      rect.data.foreach(r => {
        squareRectIndexMap.put(r, List(rect.index))
      })

      out ::= rect
      col = newCol

      if (col >= n) {
        row += 1
        col = 0
      }
    }

    out
  }

  def verMatrix(rooks: Rooks) = {
    def calculateRow(initRow: Int, initCol: Int) = {
      var row = initRow
      var out = List.empty[Point]
      while(row < n && rooks((row, initCol))) {
        out ::= (row, initCol)
        row += 1
      }
      rectIndex += 1
      (new Rect(out, rectIndex), row + 1)
    }

    var row = 0
    var col = 0
    var out = List.empty[Rect]

    while(row < n && col < n) {
      val point = (row, col)
      val (rect, newRow) = calculateRow(row, col)

      rect.data.foreach(p => {
        squareRectIndexMap.put(p, rect.index :: squareRectIndexMap(p))
      })

      out ::= rect
      row = newRow

      if (row >= n) {
        col += 1
        row = 0
      }
    }

    out
  }

  def createGraph(horRects: List[Rect], verRects: List[Rect]) = {
    val vertices = horRects ++ verRects
    new Graph(horRects, verRects)
  }

  val matrix = linesToMatrix(lines)
  val squares = matrixToIndices(matrix)
  val horRects = horMatrix(squares)
  val verRects = verMatrix(squares)
  val g = createGraph(horRects, verRects)

  println(s"answer is 0")
}
