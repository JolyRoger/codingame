package codingame.veryhard.knight

import codingames.veryhard.knight.Player
import codingames.veryhard.knight.Player.{h, x, y}
import org.scalatest.FlatSpec

class SolutionTests extends FlatSpec {

  val w = 18
  val h = 32
  val matrixDimension = (h, w) // row, col

  val cutMatrix = Array.fill[Boolean](h * w)(true)
  val booleanMapFunction = (unit: (Boolean, Boolean)) => unit._1 && unit._2
  val doubleMapFunction = (unit: (Double, Double)) => unit._1 - unit._2
  def toMatrix(number: Int): (Int, Int) = (number / w, number % w)
  implicit def toNumber(point: (Int, Int)): Int = if (point._1 >= h || point._2 >= w || point._1 < 0 || point._2 < 0)
    throw new IllegalArgumentException("Point parameters are out of bounds") else point._1 * w + point._2 % h

  "A solution" should "revert matrix to number" in {
    val point = (4, 2)
    val res = toNumber(point)
    Console.err.println(s"$point to number is $res")
  }

  "A solution" should "revert number to matrix" in {
    val num = 22
    val res = toMatrix(num)
    Console.err.println(s"$num to matrix is $res")
  }

  "A solution" should "return matrix of distances" in {
    val distanceMatrix = Player.distance((5,1), matrixDimension)
//    Player.printMatrix(matrixDimension, distanceMatrix)
  }

  "A solution" should "compare two matrices" in {
    val distanceMatrixPrev = Player.distance((5,1), matrixDimension)
    val distanceMatrixNext = Player.distance((0,0), matrixDimension)
    Console.err.print("PREV")
//    Player.printMatrix(matrixDimension, distanceMatrixPrev)
    Console.err.println("\n")
    Console.err.print("NEXT")
//    Player.printMatrix(matrixDimension, distanceMatrixNext)
    val processedMatrix: Array[Double] = Player.delta[Double](distanceMatrixPrev, distanceMatrixNext, doubleMapFunction).toArray
    Console.err.println("\n")
    Console.err.print("RES")
//    Player.printMatrix(matrixDimension, processedMatrix)
    Console.err.println
  }

  "A solution" should "return boolean matrix" in {
    val distanceMatrixPrev = Player.distance((5,1), matrixDimension)
    val distanceMatrixNext = Player.distance((1,5), matrixDimension)
    Console.err.print("PREV")
//    Player.printMatrix(matrixDimension, distanceMatrixPrev)
    Console.err.println("\n")
    Console.err.print("NEXT")
//    Player.printMatrix(matrixDimension, distanceMatrixNext)
    val processedMatrix: Array[Double] = Player.delta[Double](distanceMatrixPrev, distanceMatrixNext, doubleMapFunction).toArray
    Console.err.println("\n")
    Console.err.print("RES")
//    Player.printMatrix(matrixDimension, processedMatrix)
    Console.err.println

    val validityMatrix = Player.cut(processedMatrix, cutMatrix, "COLDER", booleanMapFunction).toArray
//    Player.printMatrix(matrixDimension, validityMatrix)
    Console.err.println
  }

  "A solution" should "map boolean matrix" in {
    val point = (17, 31)
    val distanceMatrixPrev = Player.distance(point, matrixDimension)
    cutMatrix(point) = false
    val distanceMatrixNext = Player.distance((10,4), matrixDimension)
    cutMatrix((10,4)) = false
    Console.err.print("PREV")
//    Player.printMatrix(matrixDimension, distanceMatrixPrev)
    Console.err.println("\n")
    Console.err.print("NEXT")
//    Player.printMatrix(matrixDimension, distanceMatrixNext)
    val processedMatrix: Array[Double] = Player.delta[Double](distanceMatrixPrev, distanceMatrixNext, doubleMapFunction).toArray
    Console.err.println("\n")
    Console.err.print("RES")
//    Player.printMatrix(matrixDimension, processedMatrix)

    val validityMatrix = Player.cut(processedMatrix, cutMatrix, "COLDER", booleanMapFunction).toArray
    Console.err.print("CUT MATRIX")
//    Player.printMatrix[Boolean](matrixDimension, cutMatrix)
    Console.err.print("VALIDITY MATRIX")
//    Player.printMatrix(matrixDimension, validityMatrix)

    val resultMatrix = Player.delta(validityMatrix, cutMatrix, booleanMapFunction).toArray
    Console.err.print("RESULT MATRIX")
//    Player.printMatrix(matrixDimension, resultMatrix)
  }
}
