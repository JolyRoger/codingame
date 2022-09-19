package codingames.veryhard.voxcodei

import math._
import scala.collection.mutable
import scala.io.Source
import scala.util._
import scala.io.StdIn._

object Player extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
  val filename = "resources/voxcodei/one-moving-node.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines
  def readInt = if (data.hasNext) data.next.toInt else { System.exit(0); -1 }
  def readLine = if (data.hasNext) data.next else { System.exit(0); "" }
//----------------------------------------------------------------------------------------------------------------------
  type Field = Array[Int]

  val AIR = 0
  val BOMB = 4
  val STONE = -2
  val TARGET = -1
  val WAIT = -1
  val NO_VALUE = Int.MinValue

  val STABLE = 0
  val UP = 1
  val DOWN = 2
  val LEFT = 3
  val RIGHT = 3

  val symMap = Map('.' -> AIR, '@' -> TARGET, '#' -> STONE)

  class Target(id: Int) {
    var (x,y) = toMatrix(id)
    var movement = STABLE
  }

  var firstStep = true
  var secondStep = false
  var fields: Array[Field] = _
  var totalRounds = -1
  var currentRound = 0
  var targetMap = Map.empty[Int, Target]

  val Array(width, height) = (readLine split " ").filter(_ != "").map (_.toInt)
  Console.err.println(s"$width $height")

  def toMatrix(number: Int): (Int, Int) = (number % width, number / width)
  implicit def toNumber(point: (Int, Int)): Int = point._2 * width + point._1 % width

  def adjacent(node: Int) = {
    val (x, y) = toMatrix(node)
    List((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)).withFilter { xy =>
      xy._1 >= 0 &&
      xy._1 < width &&
      xy._2 >= 0 &&
      xy._2 < height
    }.map(toNumber)
  }

  def getNextCandidate(node: Int, matrix: Field) = adjacent(node).filter(matrix(_) == TARGET).toSet

  for (_ <- 0 to 30) {
    val Array(rounds, bombs) = (readLine split " ").filter(_ != "").map (_.toInt)
    Console.err.println(s"$rounds $bombs")

    val matrix = (for(_ <- 0 until height) yield readLine).toArray.flatMap(_.map(symMap(_)).toArray)

    if (currentRound == 0) {
      fields = new Array(rounds)
      totalRounds = rounds
      targetMap = matrix.zipWithIndex.withFilter(_._1 == TARGET).map(index => (index._2, new Target(toMatrix(index._2)))).toMap
      fields(currentRound) = matrix
    } else if (currentRound == 1) {
      val candidates = targetMap.keys.collect {
        case node if matrix(node) != TARGET => (node, getNextCandidate(node, matrix))
      }
      val (unknown, known) = candidates.partition(_._2.size > 1)
      fields(currentRound) = matrix
    }

    currentRound += 1
    println("WAIT")
  }
}