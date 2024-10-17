package codingames.veryhard.voxcodei

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
//import scala.io.StdIn.readLine

object Player extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
  val filename = "resources/voxcodei/vandalism.txt"
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
    val LEFT, RIGHT, UP, DOWN, STABLE  = Value
  }

  import Direction._

  var needToCalculate = true
  val AIR = 0
  val BOMB = 4
  val STONE = -2
  val TARGET = -1
  val WAIT = -1
  val NO_VALUE = Int.MinValue
  var initialMatrix: Array[Int] = _

  var step = 0
  var recognition = false

  case class Stone(x: Int, y: Int)
  case class Target(position: Int, direction: Set[Direction])

  val symMap = Map('.' -> AIR, '@' -> TARGET, '#' -> STONE)
  val dataMap = mutable.Map.empty[Int, Link]
  val consumedStates = mutable.Set.empty[Int]
  var targetList: List[Target] = _
  var stoneSet: Set[(Int, Int)] = _

  def oppositeDirection(direction: Direction): Direction = direction match {
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
  private def movementPrediction(pos: Int, step: Int, currentDirection: Direction): Int = {
    if (step > 0) {
      var localStep = step
      var localCurrentDirection = currentDirection
      val xy = toMatrix(pos)
      val move = moveOne(xy, currentDirection).getOrElse {
        localCurrentDirection = oppositeDirection(currentDirection)
        moveOne(xy, localCurrentDirection).getOrElse {
          localStep = 0
          xy
        }
      }
      movementPrediction(move, localStep - 1, localCurrentDirection)
    } else pos
  }

  private def leftFilter(pos: Int, matrix: Array[Int], step: Int) = matrix(movementPrediction(pos, step, LEFT)) == TARGET
  private def rightFilter(pos: Int, matrix: Array[Int], step: Int) = matrix(movementPrediction(pos, step, RIGHT)) == TARGET
  private def upFilter(pos: Int, matrix: Array[Int], step: Int) = matrix(movementPrediction(pos, step, UP)) == TARGET
  private def downFilter(pos: Int, matrix: Array[Int], step: Int) = matrix(movementPrediction(pos, step, DOWN)) == TARGET
  private def stableFilter(pos: Int, matrix: Array[Int], step: Int) = matrix(pos) == TARGET

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
        Target(target.position, target.direction.filter(directionFilter(_)(target.position, matrix, step))))
    filteredTargets ::: definedTargets
  }

// ---------------------------------------------------------------------------------------------------------------------
  for (step <- LazyList.from(0).takeWhile(_ => true)) {
    val Array(rounds, bombs) = (readLine split "\\s").withFilter(_.nonEmpty).map (_.toInt)
    Console.err.println(s"$rounds $bombs")

    val lines = (for(_ <- 0 until height) yield readLine)
    lines.foreach(Console.err.println)
    val matrix = lines.flatMap(_.map(symMap(_))).toArray

    if (step == 0) {
      initialMatrix = matrix.clone
      val indicedMatrix = initialMatrix.zipWithIndex
      stoneSet = indicedMatrix.withFilter(symIndex => symIndex._1 == STONE).map(symIndex => toMatrix(symIndex._2)).toSet
      targetList = indicedMatrix
        .withFilter(symIndex => symIndex._1 == TARGET)
        .map(symIndex => Target(symIndex._2, Direction.values.filter(canMoveTo(symIndex._2, _)))).toList
    }

    if (recognition) {
      val dubiousTargets = targetList.filter(target => target.direction.size > 1)
      Console.err.println(s"recognition! step=$step")
      dubiousTargets.foreach(trg => Console.err.println(s"\t${trg.position} can be ${trg.direction.mkString(", ")}"))
      targetList = recognizeMovement(matrix, step)
    } else {
      targetList.foreach(trg => Console.err.println(s"\t${trg.position} is ${trg.direction.mkString(",")}"))
    }

    println("WAIT")

    recognition = targetList.exists(target => target.direction.size > 1)
  }
}
