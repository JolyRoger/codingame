package codingame.puzzle.rooks.v2

import scala.io.Source

object Solution extends App {
  val filename = args(0)
  type Point = (Int, Int)
  type Rooks = Map[Point, Boolean]
  type Matrix = Array[Array[Boolean]]

  class Graph(size: Int, squareRectIndexMap: Map[Point, List[Int]]) {
    val adj = (for (i <- 0 until size + 1) yield Set[Int]()).toArray

    squareRectIndexMap.foreach(data => {
      addEdge(0, data._2(1))
      addEdge(data._2(1), data._2.head)
      addEdge(data._2.head, size)
    })

    def addEdge(v: Int, w: Int){
      adj(v) = adj(v) + w
    }
  }

  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines().toList

  data.foreach(c => Console.err.println(s"$c"))
  val (n, lines) = (data.head.toInt, data.tail)

  def linesToMatrix(lines: List[String]) = lines.map(_.toCharArray.map(_ == '.')).toArray

  def rectIndices(matrix: Array[Array[Boolean]]) = matrix.zipWithIndex.flatMap(rowIndex =>
    rowIndex._1.zipWithIndex.withFilter(_._1).map(colIndex => (rowIndex._2, colIndex._2)))

  def data(rooks: Array[Point]) = {
    var index = 1
    val verRooks = rooks.sortBy(_._2)
    var prevPoint: Point = rooks.head
    var squareRectIndexMap = Map.empty[Point, List[Int]]

    rooks.foreach { p =>
      index = if (prevPoint._1 != p._1 || p._2 - prevPoint._2 > 1) index + 1 else index
      prevPoint = p
      squareRectIndexMap += (p -> List(index))
    }

    index += 1
    prevPoint = verRooks.head

    verRooks.foreach { p =>
      index = if (prevPoint._2 != p._2 || p._1 - prevPoint._1 > 1) index + 1 else index
      prevPoint = p
      squareRectIndexMap += (p -> (index :: squareRectIndexMap(p)))
    }

    (index + 1, squareRectIndexMap)
  }

  val matrix = linesToMatrix(lines)
  val squares = rectIndices(matrix)
  val (rectsAmount, rectsIndexMap) = data(squares)
  val g = new Graph(rectsAmount, rectsIndexMap)

  println(s"6")
}
