package codingames.puzzle.rooks

import scala.collection.{SortedMap, mutable}

object Solution extends App {
  type Rooks = collection.mutable.Map[(Int, Int), Boolean]

  val n = readInt

  Console.err.println(s"$n")

  val lines = (for (i <- 0 until n) yield readLine).toList

  lines.foreach(a => Console.err.println(s"$a"))

  def linesToMatrix(lines: List[String]) = lines.map(_.toCharArray.map(_ == '.')).toArray

  def matrixToIndices(matrix: Array[Array[Boolean]]) = mutable.Map(matrix.zipWithIndex.flatMap(rowIndex => rowIndex._1.zipWithIndex.map(colIndex => ((rowIndex._2, colIndex._2), colIndex._1))).toMap.toSeq: _*)

  def strikeOut(x: Int, y: Int, data: Rooks) = {
    var lim = 0

    var i = x
    while (i >= lim && data.get((i, y)).isDefined) {
      data((i, y)) = false
      i -= 1
    }

    i = y
    while (i >= lim && data.get((x, i)).isDefined) {
      data((x, i)) = false
      i -= 1
    }

    i = x
    lim = n
    while (i < lim && data.get((i, y)).isDefined) {
      data((i, y)) = false
      i += 1
    }

    i = y
    while (i < lim && data.get((x, i)).isDefined) {
      data((x, i)) = false
      i += 1
    }
    data.filter(_._2)
  }

  def maxFor(data: Rooks): Int = {
    if (data.isEmpty) 0
    else if (data.size == 1) 1
    else {
      val sm = SortedMap[(Int, Int), Boolean]() ++ data
      val rooksAmount = sm.map(square => {
        val newData = strikeOut(square._1._1, square._1._2, data)
        1 + maxFor(newData)
      })
      rooksAmount.max
    }
  }

  val res = linesToMatrix(lines)
  val res2 = matrixToIndices(res)
  val res3 = res2.filter(_._2)
  val res5 = maxFor(res3)

  Console.err.println(s"answer is $res5")
  println(res5)
}