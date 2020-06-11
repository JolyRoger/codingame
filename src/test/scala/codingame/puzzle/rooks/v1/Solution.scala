package codingame.puzzle.rooks.v1

import scala.collection.mutable
import scala.io.Source

object Solution extends App {
  val filename = "rooks/rooks3.txt"
  val bufferedSource = Source.fromFile(filename)

  val data = bufferedSource.getLines().toList

  data.foreach(c => Console.err.println(s"$c"))
  val (n, lines) = (data.head.toInt, data.tail)

  type RookStruc = mutable.Map[Rooks, List[Rooks]]

  type Rooks = Map[(Int, Int), Boolean]
//  type Init = Map[(Int, Int), Boolean]

  def print(data: Rooks) = {
    for (i <- 0 until n; j <- 0 until n) {
      if (j == 0) Console.err.println
      Console.err.print(s"${if (data.get((i, j)).isDefined && data((i, j))) '.' else 'X'}")
    }
  }

  def linesToMatrix(lines: List[String]) = lines.map(_.toCharArray.map(_ == '.')).toArray
  def rooksToMutableMap(mapMatrix: Rooks) = mutable.Map(mapMatrix.toSeq: _*)
  def matrixToIndices(matrix: Array[Array[Boolean]]) = matrix.zipWithIndex.flatMap(rowIndex => rowIndex._1.zipWithIndex.map(colIndex => ((rowIndex._2, colIndex._2), colIndex._1))).toMap
  def strikeOut(x: Int, y: Int, data: Rooks, initial: Rooks) = {
    val mData = rooksToMutableMap(data)
    var lim = 0

    var i = x
    while (i >= lim && initial.get((i, y)).isDefined) {
      mData((i, y)) = false
      i -= 1
    }

    i = y
    while (i >= lim && initial.get((x, i)).isDefined) {
      mData((x, i)) = false
      i -= 1
    }

    i = x
    lim = n
    while (i < lim && initial.get((i, y)).isDefined) {
      mData((i, y)) = false
      i += 1
    }

    i = y
    while (i < lim && initial.get((x, i)).isDefined) {
      mData((x, i)) = false
      i += 1
    }
    mData.filter(_._2).toMap
  }

/*
  for (i <- 0 until n; j <- 0 until n) {
    val res = linesToMatrix(lines)
    val res2 = matrixToIndices(res).filter(_._2)
    val res25 = mapToRooks(res2)
    val res3 = res25.filter(_._2)
    val nm = strikeOut(i, j, res3, res2)
    Console.err.print(s"\n$i $j striked out\n-------------------------")
    print(nm)
    Console.err.println
  }
*/
  var struct: RookStruc = mutable.Map.empty

  def maxFor(data: Rooks, init: Rooks): Int = {
    if (data.isEmpty) 0
    else if (data.size == 1) 1
    else {
      var max = 0
      var newData: Rooks = data
      var stack: List[Rooks] = newData :: List.empty
//      var square = newData.headOption
//      sm = sm.tail

      while (stack.nonEmpty) {
        val ddata = stack.head
        stack = stack.tail
        val positions = ddata.map(square => strikeOut(square._1._1, square._1._2, ddata, init)).toList
        struct.put(ddata, positions)
        stack = positions ::: stack
      }

      /*
      while (stack.nonEmpty) {
        val square = stack.head.head
        stack = stack.tail
        newData = strikeOut(square._1._1, square._1._2, newData, init)
        if (newData.nonEmpty) stack = newData :: stack
        square = newData.headOption
      }

      max = Math.max(max, stack.size)

      val rooksAmount = sm.map(square => {
        val newData = strikeOut(square._1._1, square._1._2, data, init)
//        Console.err.print(s"${square._1._1}, ${square._1._2} striked out")
//        print(newData)
//        Console.err.println
        1 + maxFor(newData, init)
      })
*/
//      rooksAmount.max
      0
    }
  }

  val init = matrixToIndices(linesToMatrix(lines)).filter(_._2)
  val res5 = maxFor(init, init)

  Console.err.println
  Console.out.println
  println(s"answer is $res5")
}
