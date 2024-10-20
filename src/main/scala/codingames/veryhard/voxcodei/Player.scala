package codingames.veryhard.voxcodei

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.io.StdIn.readLine

object Player extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
    val filename = "resources/voxcodei/vandalism.txt"
//    val filename = "resources/voxcodei/one-moving-node.txt"
    val bufferedSource = Source.fromFile(filename)
    val data = bufferedSource.getLines
    def readInt = if (data.hasNext) data.next.toInt else { System.exit(0); -1 }
    def readLine = if (data.hasNext) data.next else { System.exit(0); "" }
//----------------------------------------------------------------------------------------------------------------------

  type Field = Array[Int]
  type State = (Field, Int, Int)    // field, rounds, bombs
  type Link = (Int, Int)            // from, prev state hash
  type LinkedState = (State, Link)  // state, link
  type DirectionFilter = (Int, Array[Int], Int) => Boolean

  object Direction extends Enumeration {
    type Direction = Value
    val LEFT, RIGHT, UP, DOWN, STABLE, UNKNOWN  = Value
    val moves = Set(LEFT, RIGHT, UP, DOWN, STABLE)
  }

  import Direction._

  var needToCalculate = true
  val AIR = 0
  val BOMB = 4
  val STONE = -2
  val TARGET_UNKNOWN = -1
  val TARGET_LEFT = -10
  val TARGET_RIGHT = -11
  val TARGET_UP = -12
  val TARGET_DOWN = -13
  val TARGET_STABLE = -14
  val WAIT = -1
  val NO_VALUE = Int.MinValue
  var initialMatrix: Array[Int] = _

  var step = 0
  var recognition = true
  var firstRecognition = true

  case class Target(position: Int, initialPosition: Int, direction: Set[Direction], realDirection: Direction)

  val directionMap = Map(LEFT -> TARGET_LEFT, RIGHT -> TARGET_RIGHT, UP -> TARGET_UP, DOWN -> TARGET_DOWN, STABLE -> TARGET_STABLE)
  val symMap = Map('.' -> AIR, '@' -> TARGET_UNKNOWN, '#' -> STONE)
  val dataMap = mutable.Map.empty[Int, Link]
  val consumedStates = mutable.Set.empty[Int]
  var targetList: List[Target] = _
  var targetMap: Map[Int, Target] = _
  var stoneSet: Set[(Int, Int)] = _

  private def ofMap(line: String, i: Int) = line.map(symMap)

  private def targetDefinition(pos: Int, sym: Char) = {
    val t = symMap(sym)
    if (t != TARGET_UNKNOWN) t
    else directionMap(targetMap(pos).direction.head)
  }

  private def oppositeDirection(direction: Direction): Direction = direction match {
    case LEFT => RIGHT
    case RIGHT => LEFT
    case UP => DOWN
    case DOWN => UP
  }

  private def moveOne(xy: (Int, Int), direction: Direction): Option[(Int, Int)] = direction match {
    case LEFT =>
      val newX = xy._1 - 1
      if (newX < 0 || stoneSet.contains(newX, xy._2)) None else Some(newX, xy._2)
    case RIGHT =>
      val newX = xy._1 + 1
      if (newX >= width || stoneSet.contains(newX, xy._2)) None else Some(newX, xy._2)
    case UP =>
      val newY = xy._2 - 1
      if (newY < 0 || stoneSet.contains(xy._1, newY)) None else Some(xy._1, newY)
    case DOWN =>
      val newY = xy._2 + 1
      if (newY >= height || stoneSet.contains(xy._1, newY)) None else Some(xy._1, newY)
    case STABLE => Some(xy)
  }

  private def canMoveTo(pos: Int, direction: Direction) = moveOne(toMatrix(pos), direction).isDefined

  @tailrec
  private def movementPrediction(pos: Int, step: Int, direction: Direction): (Direction, Int) = {
    if (step > 0) {
      var localStep = step
      var realDirection = direction
      val xy = toMatrix(pos)
      val move = moveOne(xy, direction).getOrElse {
        realDirection = oppositeDirection(direction)
        moveOne(xy, realDirection).getOrElse {
          localStep = 0
          xy
        }
      }
      movementPrediction(move, localStep - 1, realDirection)
    } else {
      (if (canMoveTo(pos, direction)) direction else oppositeDirection(direction), pos)
    }
  }

  private def leftFilter(pos: Int, matrix: Array[Int], step: Int) = matrix(movementPrediction(pos, step, LEFT)._2) == TARGET_UNKNOWN
  private def rightFilter(pos: Int, matrix: Array[Int], step: Int) = matrix(movementPrediction(pos, step, RIGHT)._2) == TARGET_UNKNOWN
  private def upFilter(pos: Int, matrix: Array[Int], step: Int) = matrix(movementPrediction(pos, step, UP)._2) == TARGET_UNKNOWN
  private def downFilter(pos: Int, matrix: Array[Int], step: Int) = matrix(movementPrediction(pos, step, DOWN)._2) == TARGET_UNKNOWN
  private def stableFilter(pos: Int, matrix: Array[Int], step: Int) = matrix(pos) == TARGET_UNKNOWN

  val directionFilter: Map[Direction, DirectionFilter] = Map(LEFT -> leftFilter,
    RIGHT -> rightFilter,
    UP -> upFilter,
    DOWN -> downFilter,
    STABLE -> stableFilter)

  val Array(width, height) = (readLine split "\\s").withFilter(_.nonEmpty).map(_.toInt)
  Console.err.println(s"$width $height")

  def toMatrix(number: Int): (Int, Int) = (number % width, number / width)
  implicit def toNumber(point: (Int, Int)): Int = point._2 * width + point._1 % width

  private def recognizeMovement(matrix: Array[Int], step: Int) = {
    val (undefinedTargets, definedTargets) = targetList.partition(_.direction.size > 1)
    val filteredTargets = undefinedTargets.map(target =>
      Target(target.initialPosition, target.initialPosition, target.direction.filter(directionFilter(_)(target.position, matrix, step)), UNKNOWN))
    filteredTargets ::: definedTargets
  }

  private def simplifiedEntry = {
    val Array(rounds, bombs) = (readLine split "\\s").withFilter(_.nonEmpty).map(_.toInt)
    val lines = for(_ <- 0 until height) yield readLine
    (rounds, bombs, lines)
  }

  private def entry(lineProcess: (String, Int) => IndexedSeq[Int]) = {
    val Array(rounds, bombs) = (readLine split "\\s").withFilter(_.nonEmpty).map(_.toInt)
    Console.err.println(s"$rounds $bombs")

    val lines = for(i <- 0 until height) yield {
      val line = readLine
      Console.err.println(s"$line")
//      line.zipWithIndex.map(symIndex => targetDefinition(recognition, step, (symIndex._2, i), symIndex._1))
//      line.map(a => symMap(a))
      lineProcess(line, i)
    }
    val matrix = lines.flatten.toArray

    (rounds, bombs, matrix)
  }

  private def firstStep(matrix: Array[Int]) = {
    initialMatrix = matrix.clone
    val indicedMatrix = initialMatrix.zipWithIndex
    stoneSet = indicedMatrix.withFilter(symIndex => symIndex._1 == STONE).map(symIndex => toMatrix(symIndex._2)).toSet
    targetList = indicedMatrix
      .withFilter(symIndex => symIndex._1 == TARGET_UNKNOWN)
      .map(symIndex => Target(symIndex._2, symIndex._2, Direction.moves.filter(canMoveTo(symIndex._2, _)), UNKNOWN)).toList
    targetMap = targetList.map(target => (target.initialPosition, target)).toMap
    println("WAIT")
  }

  firstStep(entry(ofMap)._3)

// ---------------------------------------------------------------------------------------------------------------------
  for (step <- LazyList.from(1).takeWhile(_ => true)) {
    recognition = recognition && targetList.exists(target => target.direction.size > 1)

    if (recognition) {
      val (rounds, bombs, matrix) = entry(ofMap)
      val dubiousTargets = targetList.filter(target => target.direction.size > 1)
      Console.err.println(s"recognition! step=$step")
      dubiousTargets.foreach(trg => Console.err.println(s"\t${trg.position} can be ${trg.direction.mkString(", ")}"))
      targetList = recognizeMovement(matrix, step)
    } else {
      val (_, _, lines) = simplifiedEntry
      lines.foreach(Console.err.println)
      val (stableTargetList, movedTargetList) = targetList.partition(_.direction.head == STABLE)
      targetList = if (firstRecognition)
        stableTargetList.map(target => Target(target.position, target.initialPosition, target.direction, STABLE)) :::
        movedTargetList.map(target => {
          val (direction, position) = movementPrediction(target.initialPosition, step, target.direction.head)
          Target(position, target.initialPosition, target.direction, direction)}) else
        stableTargetList ::: movedTargetList.map(target => {
          val (direction, position) = movementPrediction(target.position, 1, target.direction.head)
          Target(position, target.initialPosition, target.direction, direction)
        })

      firstRecognition = false
      targetList.foreach(trg => Console.err.println(s"\t${trg.position} is ${trg.direction.head}"))
    }
    println("WAIT")
  }
}
