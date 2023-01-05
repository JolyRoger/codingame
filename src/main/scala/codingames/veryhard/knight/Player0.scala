package codingames.veryhard.knight

import math._
import scala.io.Source
import scala.io.StdIn._
import scala.reflect.ClassTag

object Player0 extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
//   val filename = "resources/knight/lot-of-jumps.txt"
   val filename = "resources/knight/tower.txt"
  // val filename = "resources/knight/lesser-jumps.txt"
  //   val filename = "resources/knight/lot-of-windows.txt"
    val bufferedSource = Source.fromFile(filename)
    val data = bufferedSource.getLines
    def readInt = if (data.hasNext) data.next.toInt else {System.exit(0); -1}
    def readLine = if (data.hasNext) data.next else {System.exit(0); ""}
// ----------------------------------------------------------------------------------------------------------------------

  val UNKNOWN = 0
  val SAME = 1
  val WARMER = 2
  val COLDER = 3

  val bombDirectionMap = Map("UNKNOWN" -> UNKNOWN, "SAME" -> SAME, "WARMER" -> WARMER, "COLDER" -> COLDER)
  val bombDirectionArr = Array("UNKNOWN", "SAME", "WARMER", "COLDER")                                                   // FIXME: only for print
  val Array(w, h) = for (i <- readLine split " ") yield i.toInt
  val n = readInt
  val Array(x0, y0) = for (i <- readLine split " ") yield i.toInt
  var bombDirection = bombDirectionMap("UNKNOWN")

  val dimension = (w, h)
  var prevX = -1
  var prevY = -1
  var x = x0
  var y = y0
  var cutMatrix = Array.empty[Boolean]

  var findY = false
  var oldAlgorithm = false

  var xSize = -1
  var xOffset = -1
  var ySize = -1
  var yOffset = -1

  var lowLimit = 0
  var upLimit = if (findY) h - 1 else w - 1
  var size = -1
  var visited = Set.empty[Int]


  def toMatrix(number: Int): (Int, Int) = (number % xSize, number / xSize)
  implicit def toNumber(point: (Int, Int)): Int = point._2 * xSize + point._1 % xSize
  def ~=(x: Float, y: Float, precision: Float) = (x - y).abs < precision
  def delta(newMatrix: Array[Boolean]) = (for (i <- cutMatrix.indices) yield cutMatrix(i) && newMatrix(i)).toArray

  def printMatrix[T](dimension: (Int, Int), matrix: Array[T]) = {
    for (j <- 0 until dimension._2;i <- 0 until dimension._1) {
      Console.err.print(s"${if (i == 0) "\n" else ""}")
      matrix(toNumber((i, j))) match {
        case point: Float => Console.err.print(f"$point%1.2f")
        case point: Boolean => Console.err.print(s"${if (point) '*' else '#'}")
      }
    }
    Console.err.println
  }

  def newCutMatrix = {
    val k = cornerFactor(x, y, prevX, prevY)
    Console.err.println(s"calculate corner: $x, $y, $prevX, $prevY")
    if (bombDirection != UNKNOWN) {
      val correctlyFunction = getCorrectlyFunction(bombDirection, k)
      val b = getB(x, y, prevX, prevY, k)
      Console.err.println(s"Cut increment=$b")
      (for (index <- 0 until size) yield {
        val (_x, _y) = toMatrix(index)
        correctlyFunction(_y, _x, k, b)
      }).toArray
    } else Array.empty[Boolean]
  }

  def findPoint(uncheckedPoint: Array[Int], cutMatrix: Array[Boolean]): (Int, Int) = {
    val mpoints = uncheckedPoint.map(point => toMatrix(point))
    val maxXIndex = mpoints.maxBy(_._1)._1
    val minXIndex = mpoints.minBy(_._1)._1
    val maxYIndex = mpoints.maxBy(_._2)._2
    val minYIndex = mpoints.minBy(_._2)._2
    val mediX = minXIndex + Math.round(maxXIndex - minXIndex) / 2
    val mediY = minYIndex + Math.round(maxYIndex - minYIndex) / 2
    var counter = 0
    var point = (-1,-1)

    while (point._1 == -1) {
      if (cutMatrix((mediX, mediY))) {
        point = (mediX, mediY)
      } else if (cutMatrix((mediX + counter, mediY))) {
        point = (mediX + counter, mediY)
      } else if (cutMatrix((mediX, mediY + counter))) {
        point = (mediX, mediY + counter)
      } else if (cutMatrix((mediX - counter, mediY))) {
        point = (mediX - counter, mediY)
      } else if (cutMatrix((mediX, mediY - counter))) {
        point = (mediX, mediY - counter)
      } else if (cutMatrix((mediX + counter, mediY - counter))) {
        point = (mediX + counter, mediY - counter)
      } else if (cutMatrix((mediX - counter, mediY + counter))) {
        point = (mediX - counter, mediY + counter)
      } else if (cutMatrix((mediX + counter, mediY + counter))) {
        point = (mediX + counter, mediY + counter)
      } else if (cutMatrix((mediX - counter, mediY - counter))) {
        point = (mediX - counter, mediY - counter)
      } else counter += 1
    }
    point
  }

  def firstMove(x: Int, y: Int) = {
    val indexCandidate = (xSize / 2, ySize / 2)
    if (indexCandidate == (x, y)) (indexCandidate._1 + 1, indexCandidate._2) else indexCandidate
  }

  def nextMove(x: Int, y: Int, bombDirection: Int): (Int, Int) = {
    if (bombDirection == UNKNOWN) {
      val index = firstMove(x, y)
      cutMatrix(index) = false
      index
    } else {
      cutMatrix = delta(newCutMatrix)
      val uncheckedPoint = cutMatrix.zipWithIndex.withFilter(_._1).map(_._2)
      val index = findPoint(uncheckedPoint, cutMatrix)
      cutMatrix(index) = false
      index
    }
  }

  def getB(x: Float, y: Float, prevX: Float, prevY: Float, k: Float) = halfDistance(y, prevY) - k * halfDistance(x, prevX)
  def cornerFactor(x: Float, y: Float, prevX: Float, prevY: Float) = -1 / ((y - prevY) / (x - prevX))
  def halfDistance(a: Float, b: Float) = Math.min(a, b) + Math.abs(a - b) / 2
  def getCorrectlyFunction(bombDirection: Int, k: Float): (Int, Int, Float, Float) => Boolean = {
    if (bombDirection == SAME) {
      if (k == Float.PositiveInfinity || k == Float.NegativeInfinity) {
        val halfX = halfDistance(x, prevX).toInt
        (_,x,_,_) => x == halfX
      } else {
        (y,x,k,b) => ~=(y, k * x + b, 0.01f)
      }
    } else
      if (k == Float.PositiveInfinity || k == Float.NegativeInfinity) {
        val halfX = halfDistance(x, prevX)
        if ((bombDirection == WARMER) == (prevX < x)) (_,x,_,_) => x > halfX
        else (_,x,_,_) => x < halfX
      } else if ((bombDirection == WARMER) == (prevY < y))
        (y,x,k,b) => y > k * x + b else
        (y,x,k,b) => y < k * x + b
  }

  //  for (_ <- LazyList.from(0).takeWhile(_ < n)) {
  //    bombDirection = bombDirectionMap(readLine)
  //    Console.err.println(s"${bombDirectionArr(bombDirection)}")
  //
  //    val (x_, y_) = nextMove(bombDirection)
  //    prevX = x
  //    prevY = y
  //    x = x_
  //    y = y_
  //
  //    println(s"$x $y")
  //  }











//  val coeff = Array(1.2f, 5.0f, 2.0f)

  def newVal(lowLimit: Int, upLimit: Int, toRight: Boolean, step: Int) = {
    if (lowLimit == upLimit) lowLimit
    else {
      var nv = -1
      var count = 0
      val shouldExit: Int => Boolean = if (toRight) _ != upLimit else _ != lowLimit
      do {
        nv = lowLimit + Math.round((upLimit - lowLimit) / /*coeff(step % 3)*/ 2)
        nv = if (toRight) nv + count else nv - count
        count += 1
      } while(visited.contains(nv) && shouldExit(nv))
      nv
    }
  }

  def newColderLimits(z: Int, prevz: Int, low: Int, up: Int, step: Int) = {
    val (newZ, newLowerLimit, newUpperLimit) = if (z > prevz) {
      val _upLimit = Math.min(z - ((z - prevz) / 2) - 1, up)
      (newVal(low, _upLimit, true, step), low, _upLimit)
    } else if (z < prevz) {
      val _lowLimit = Math.max(1 + z + ((z - prevz) / 2), low)
      (newVal(_lowLimit, up, false, step), _lowLimit, up)
    } else (-1, -1, -1)
    (newZ, newLowerLimit, newUpperLimit)
  }

  def newWarmerLimits(z: Int, prevz: Int, low: Int, up: Int, step: Int) = {
    val (newZ, newLowerLimit, newUpperLimit) = if (z > prevz) {
      val _lowLimit = Math.max(1 + prevz + ((z - prevz) / 2), low)
      (newVal(_lowLimit, up, true, step), _lowLimit, up)
    } else if (z < prevz) {
      val _upLimit = Math.min(prevz - ((prevz - z) / 2), up)
      (newVal(low, _upLimit, false, step), low, z - ((z - prevz) / 2))
    } else (-1, -1, -1)
    (newZ, newLowerLimit, newUpperLimit)
  }

  def firstX(z: Int, lowerLimit: Int, upperLimit: Int) = {
    val newz = (upperLimit - lowerLimit) / 2
    if (newz == z) (newz - 1, lowerLimit, upperLimit) else (newz, lowerLimit, upperLimit)
  }

  def newCoord(genx: Int, prevx: Int, lowerLimit: Int, upperLimit: Int, bombdir: Int, step: Int) = {
    if (lowerLimit == upperLimit) (genx, lowerLimit, upperLimit)
    else if (bombdir == UNKNOWN)
      firstX(genx, lowerLimit, upperLimit)
    else {
      if (bombdir == WARMER) {
        newWarmerLimits(genx, prevx, lowerLimit, upperLimit, step)
      } else if (bombdir == COLDER) {
        newColderLimits(genx, prevx, lowerLimit, upperLimit, step)
      } else (0, lowerLimit, upperLimit)
    }
  }

  def checkConditions(bd: Int): Int = {
    if (!oldAlgorithm && upLimit - lowLimit < 51) {
      if (!findY) {
        findY = true
        xSize = upLimit - lowLimit + 1
        xOffset = lowLimit
        lowLimit = 0
        upLimit = h - 1
        Console.err.println("FINDY!!!")
        visited = Set.empty
        return checkConditions(UNKNOWN)
      } else {
        oldAlgorithm = true
        ySize = upLimit - lowLimit + 1
        yOffset = lowLimit
        size = xSize * ySize
        cutMatrix = Array.fill[Boolean](size)(true)
        Console.err.println(s"OLDALGO!!! xSize=$xSize ySize=$ySize xOffset=$xOffset yOffset=$yOffset")
        return UNKNOWN
      }
    }
    bd
  }

  for (step <- LazyList.from(0).takeWhile(_ < n)) {
    bombDirection = bombDirectionMap(readLine)
    Console.err.println(bombDirectionArr(bombDirection))

    bombDirection = checkConditions(bombDirection)

    if (oldAlgorithm) {
      val (x_, y_) = nextMove(x - xOffset, y - yOffset, bombDirection)
      prevX = x
      prevY = y
      x = x_
      y = y_
      println(s"${x + xOffset} ${y + yOffset}")
      if (cutMatrix.nonEmpty) {
        Console.err.println(s"NEW CUT MATRIX!!")
        printMatrix((xSize, ySize), cutMatrix)
      }
    } else {
      val (_z, _lowLimit, _upLimit) = if (findY) newCoord(y, prevY, lowLimit, upLimit, bombDirection, step)
      else newCoord(x, prevX, lowLimit, upLimit, bombDirection, step)
      Console.err.println(s"<${_lowLimit}..${_upLimit}>\tprevZ=${if (findY) y else x} newZ=${_z}")
      lowLimit = _lowLimit
      upLimit = _upLimit
      if (findY) {
        prevY = y
        y = _z
        visited += y
      } else {
        prevX = x
        x = _z
        visited += x
      }

      println(s"$x $y")
    }
  }
}