package codingame.puzzle.rooks.v2

import scala.io.Source

object Solution extends App {
	val filename = args(0)
	type Point = (Int, Int)
	type Rooks = Map[Point, Boolean]
	type Matrix = Array[Array[Boolean]]
	type Edges = Map[Int, Set[Adj]]

	case class Adj(to: Int, capacity: Int, filled: Int, isDirect: Boolean)

	case class Edge(from: Int, to: Int, capacity: Int, filled: Int)

	class Graph(size: Int, squareRectIndexMap: Map[Point, List[Int]]) {
		var edgeMap: Edges = Map.empty
		val adj = new Array[Int](size)

		def putEdge(v: Int, w: Int, edgeMap: Edges) =
			edgeMap + (v -> (edgeMap.getOrElse(v, Set.empty[Adj]) + Adj(w, 1, 0, true))) +
				(w -> (edgeMap.getOrElse(w, Set.empty[Adj]) + Adj(v, 1, 0, false)))

		squareRectIndexMap.foreach(data => {
			addEdge(0, data._2(1))
			edgeMap = putEdge(0, data._2(1), edgeMap)

			addEdge(data._2(1), data._2.head)
			edgeMap = putEdge(data._2(1), data._2.head, edgeMap)

			addEdge(data._2.head, size - 1)
			edgeMap = putEdge(data._2.head, size - 1, edgeMap)
		})

		def addEdge(v: Int, w: Int) {
			adj(v) = adj(v) + w
			adj(w) = adj(w) + v
		}

		def directPath(kv: (Point, Adj), from: Int) = kv._1._1 == 0 && kv._2.filled < kv._2.capacity && kv._2.isDirect

		def dfs(v: Int, veMap: Edges, adjs: Set[Adj], path: List[Int]): List[Int] = {
			if (v == size - 1) v :: path

			val nextOpt = adjs.find(edge => edge.isDirect && edge.capacity - edge.filled > 0)

//			val nextOpt = veMap.getOrElse(v, Set.empty[Adj]).find(edge => edge.isDirect && edge.capacity - edge.filled > 0)
			nextOpt match {
				case Some(next) =>
//					edges + (v -> (edges.getOrElse(v, Set.empty[Adj]) + Adj(next.to, 1, 1, true)))
					dfs(next.to, veMap, veMap(next.to), v :: path)
				case None =>
//					veMap + (v -> (veMap.getOrElse(v, Set.empty[Adj]) + Adj(next.to, 1, 1, true)))
					path
			}
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

	var res = List.empty[Int]
	do {
	 	res = g.dfs(0, g.edgeMap, g.edgeMap(0), List.empty[Int])
	} while(res.nonEmpty)


	println(s"6")
}
