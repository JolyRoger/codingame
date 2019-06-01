//package codingames.puzzle.rooks

object Solution extends App {
  type Rooks = Map[Point, Boolean]
  type Point = (Int, Int)
  val n = readInt

  Console.err.println(s"$n")

  val lines = (for (i <- 0 until n) yield readLine).toList

  lines.foreach(a => Console.err.println(s"$a"))

  def linesToMatrix(lines: List[String]) = lines.map(_.toCharArray.map(_ == '.')).toArray

  def rectIndices(matrix: Array[Array[Boolean]]) = matrix.zipWithIndex.flatMap(rowIndex =>
    rowIndex._1.zipWithIndex.withFilter(_._1).map(colIndex => (rowIndex._2, colIndex._2)))

  def data2(rooks: Array[Point]) = {
    var horIndex = 1
    var verIndex = 1
    val verRooks = rooks.sortBy(_._2)
    var prevPoint: Point = rooks.head
    var squareRectIndexMap = Map.empty[Point, List[Int]]

    rooks.foreach { p =>
      horIndex = if (prevPoint._1 != p._1 || p._2 - prevPoint._2 > 1) horIndex + 1 else horIndex
      prevPoint = p
      squareRectIndexMap += (p -> List(horIndex))
    }

    //    horIndex += 1
    prevPoint = verRooks.head

    verRooks.foreach { p =>
      verIndex = if (prevPoint._2 != p._2 || p._1 - prevPoint._1 > 1) verIndex + 1 else verIndex
      prevPoint = p
      squareRectIndexMap += (p -> (verIndex :: squareRectIndexMap(p)))
    }

    (horIndex, verIndex)
  }

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
  val (horIndex, verIndex) = data2(squares)
  //  val (rectsAmount, rectsIndexMap) = data(squares)
  //  val g = new Graph(rectsAmount, rectsIndexMap)



  println(s"${Math.min(horIndex, verIndex)}")
}
