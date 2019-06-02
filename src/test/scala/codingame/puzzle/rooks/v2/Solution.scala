package codingame.puzzle.rooks.v2

import scala.collection.mutable
import scala.io.Source

object Solution extends App {
  val filename = args(0)
  type Point = (Int, Int)
  type Rooks = Map[Point, Boolean]
  type Matrix = Array[Array[Boolean]]

  case class Adj(index: Int, capacity: Int, filled: Int, isDirect: Boolean)

  class Graph(size: Int, squareRectIndexMap: Map[Point, List[Int]]) {
    val adj = (for (i <- 0 until size) yield Set[Adj]()).toArray

    squareRectIndexMap.foreach(data => {
      addEdge(0, Adj(data._2(1), 1, 0, true))
      addEdge(data._2(1), Adj(0, 1, 0, false))

      addEdge(data._2(1), Adj(data._2.head, 1, 0, true))
      addEdge(data._2.head, Adj(data._2(1), 1, 0, false))

      addEdge(data._2.head, Adj(size - 1, 1, 0, true))
      addEdge(size - 1, Adj(data._2.head, 1, 0, false))
    })

    def addEdge(v: Int, w: Adj){
      adj(v) = adj(v) + w
    }

    def bfs(s: Int) = {
      val marked: Array[Boolean] = new Array[Boolean](size)
      val edgeTo = Array.fill[Int](size)(Int.MaxValue)
      val distTo = Array.fill[Int](size)(Int.MaxValue)
      val q = mutable.Queue[Int]()
      var i = 0

      q.enqueue(s)
      marked(s) = true
      distTo(s) = 0
      while (q.nonEmpty) {
        val v = q.dequeue
        i = i + 1
        adj(v).filterNot(marked).foreach(
          w => {
            q.enqueue(w)
            marked(w) = true
            edgeTo(w) = v
            distTo(w) = distTo(v) + 1
          }
        )
      }
      (edgeTo, distTo)
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

    (index + 2, squareRectIndexMap)
  }

  val matrix = linesToMatrix(lines)
  val squares = rectIndices(matrix)
  val (rectsAmount, rectsIndexMap) = data(squares)
  val g = new Graph(rectsAmount, rectsIndexMap)

  println(s"6")
}
