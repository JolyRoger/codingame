//package codingames.puzzle.rooks

object Solution extends App {
	type Point = (Int, Int)
	type Rooks = Map[Point, Boolean]
	type Matrix = Array[Array[Boolean]]
	type Edges = Map[Int, Set[Adj]]

	case class Adj(from: Int, to: Int, capacity: Int, filled: Int, isDirect: Boolean)

	class Graph(size: Int, squareRectIndexMap: Map[Point, List[Int]]) {
		var edgeMap: Edges = Map.empty

		def putEdge(v: Int, w: Int, edgeMap: Edges) =
			edgeMap + (v -> (edgeMap.getOrElse(v, Set.empty[Adj]) + Adj(v, w, 1, 0, true))) +
				(w -> (edgeMap.getOrElse(w, Set.empty[Adj]) + Adj(w, v, 1, 0, false)))

		squareRectIndexMap.foreach(data => {
			edgeMap = putEdge(0, data._2(1), edgeMap)
			edgeMap = putEdge(data._2(1), data._2.head, edgeMap)
			edgeMap = putEdge(data._2.head, size - 1, edgeMap)
		})

		def directPath(kv: (Point, Adj), from: Int) = kv._1._1 == 0 && kv._2.filled < kv._2.capacity && kv._2.isDirect

		def dfs2 = {
			val init = Adj(-1, 0, 0, 0, true)
			var stack = List(init)
			var path = List.empty[Int]
			var pathNotFound = true

			while (stack.nonEmpty && pathNotFound) {
				val v = stack.head
				stack = stack.tail
				path = path.dropWhile(_ > v.from)
				val ws = edgeMap(v.to).filter(edge => edge.isDirect && edge.capacity - edge.filled > 0).toList
				pathNotFound = v.to < size - 1

				stack = ws ++ stack
				path ::= v.to
			}

			(if (path.head == size - 1) {

				path = path.reverse

				for (i <- 0 until path.length - 1) {
					val v = path(i)
					val next = path(i + 1)

					val adjs = edgeMap(v)
					val newAdjs = adjs - Adj(v, next, 1, 0, true) +
						Adj(v, next, 1, 1, true)
					edgeMap = edgeMap + (v -> newAdjs)

				}
				path
			} else List.empty, edgeMap)
		}
	}

	val n = readInt

	Console.err.println(s"$n")

	val lines = (for (i <- 0 until n) yield readLine).toList

	lines.foreach(a => Console.err.println(s"$a"))

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

	var resList = List.empty[List[Int]]
	var res = List.empty[Int]

	do {
		val (newRes, newEdges) = g.dfs2
		res = newRes
		resList = if (res.isEmpty) resList else res :: resList
		g.edgeMap = newEdges
	} while (res.nonEmpty)


	println(s"${resList.length}")
}
