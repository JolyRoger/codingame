//package codingames.hard.thelastcrusade

import scala.collection.mutable.ListBuffer
import scala.io.StdIn._

object Player extends App {

  object Directive extends Enumeration {
    type Directive = Value
    val LEFT, RIGHT, TOP, WAIT, DOUBLE, NONE = Value
  }

  import Directive._

  type Point = (Int, Int)
  type NodeData = (Int, Int, Directive)
  type NextXY = Point => NodeData
  type Maze = IndexedSeq[Array[Int]]

  var i = 0
  var firstStep = true

  case class Node(x: Int, y: Int, from: Directive, command: Directive, nodeType: Int, step: Int, waitIndex: Int, parent: Node) {
    lazy val notLegalDouble = waitIndex < 0 && command == DOUBLE
    def newWaitIndex(command: Directive) = if (command == WAIT) waitIndex + 1 else if (command == Directive.DOUBLE) waitIndex - 1 else waitIndex
  }

  case class Rock(x: Int, y: Int, from: Directive, path: ListBuffer[(Int, Int, Boolean)], disposed: Boolean) {
    lazy val stopList = path.filter(_._3)
    def stopListSize = stopList.length
  }


  val WAIT_CMD = "WAIT"
  val Array(w, h) = (readLine split " ").map(_.toInt)
  val mazeData = for (_ <- 0 until h) yield readLine.split(" ").map(_.toInt)
  val exit = readLine.toInt
  val directiveConverter = Map("LEFT" -> LEFT, "RIGHT" -> RIGHT, "TOP" -> TOP)
  var pointRockMap = Map.empty[Point, Rock]

  val prevSquare: Map[Directive, NextXY] = Map(TOP -> up, LEFT -> left, RIGHT -> right)

  val connection: Array[Map[Directive, NextXY]] = Array(
    Map(),                                                                                    // 0
    Map(TOP -> down, RIGHT -> down, LEFT -> down),                                            // 1
    Map(RIGHT -> left, LEFT -> right),                                                        // 2
    Map(TOP -> down),                                                                         // 3
    Map(TOP -> left, RIGHT -> down),                                                          // 4
    Map(TOP -> right, LEFT -> down),                                                          // 5
    Map(RIGHT -> left, LEFT -> right),                                                        // 6
    Map(TOP -> down, RIGHT -> down),                                                          // 7
    Map(LEFT -> down, RIGHT -> down),                                                         // 8
    Map(TOP -> down, LEFT -> down),                                                           // 9
    Map(TOP -> left),                                                                         // 10
    Map(TOP -> right),                                                                        // 11
    Map(RIGHT -> down),                                                                       // 12
    Map(LEFT -> down),                                                                        // 13
  )

  val turn: Array[Map[Directive, List[Directive]]] = Array(
    Map(),                                                                                    // 0
    Map(TOP -> List(WAIT), RIGHT -> List(WAIT), LEFT -> List(WAIT)),                          // 1
    Map(TOP -> List(RIGHT), RIGHT -> List(WAIT), LEFT -> List(WAIT)),                         // 2
    Map(TOP -> List(WAIT), RIGHT -> List(RIGHT), LEFT -> List(LEFT)),                         // 3
    Map(TOP -> List(WAIT, LEFT), RIGHT -> List(WAIT), LEFT -> List(RIGHT)),                   // 4
    Map(TOP -> List(WAIT, RIGHT), RIGHT -> List(LEFT), LEFT -> List(WAIT)),                   // 5
    Map(TOP -> List(LEFT, RIGHT), RIGHT -> List(WAIT, RIGHT), LEFT -> List(WAIT, LEFT)),      // 6
    Map(TOP -> List(WAIT), RIGHT -> List(WAIT, LEFT), LEFT -> List(LEFT, RIGHT)),             // 7
    Map(TOP -> List(LEFT, RIGHT), RIGHT -> List(WAIT, DOUBLE), LEFT -> List(WAIT, DOUBLE)),   // 8
    Map(TOP -> List(WAIT), RIGHT -> List(LEFT, RIGHT), LEFT -> List(WAIT, RIGHT)),            // 9
    Map(TOP -> List(WAIT, RIGHT), LEFT -> List(LEFT), RIGHT -> List(DOUBLE)),                 // 10
    Map(TOP -> List(WAIT, LEFT), RIGHT -> List(RIGHT), LEFT -> List(DOUBLE)),                 // 11
    Map(TOP -> List(LEFT, DOUBLE), LEFT -> List(RIGHT), RIGHT -> List(WAIT)),                 // 12
    Map(TOP -> List(RIGHT, DOUBLE), LEFT -> List(WAIT), RIGHT -> List(LEFT))                  // 13
  )

  val newType: Array[Map[Directive, Int]] = Array(
    Map(),                                                                                    // 0
    Map(RIGHT -> -1,  LEFT -> -1 , WAIT -> -1),                                               // 1
    Map(RIGHT -> -3,  LEFT -> -3 , WAIT -> -2),                                               // 2
    Map(RIGHT -> -2,  LEFT -> -2 , WAIT -> -3),                                               // 3
    Map(RIGHT -> -5,  LEFT -> -5 , WAIT -> -4),                                               // 4
    Map(RIGHT -> -4,  LEFT -> -4 , WAIT -> -5),                                               // 5
    Map(RIGHT -> -7,  LEFT -> -9 , WAIT -> -6),                                               // 6
    Map(RIGHT -> -8,  LEFT -> -6 , WAIT -> -7),                                               // 7
    Map(RIGHT -> -9,  LEFT -> -7 , WAIT -> -8, DOUBLE -> -6),                                 // 8
    Map(RIGHT -> -6,  LEFT -> -8 , WAIT -> -9),                                               // 9
    Map(RIGHT -> -11, LEFT -> -13, WAIT -> -10, DOUBLE -> -12),                               // 10
    Map(RIGHT -> -12, LEFT -> -10, WAIT -> -11, DOUBLE -> -13),                               // 11
    Map(RIGHT -> -13, LEFT -> -11, WAIT -> -12, DOUBLE -> -10),                               // 12
    Map(RIGHT -> -10, LEFT -> -12, WAIT -> -13, DOUBLE -> -11)                                // 13
  )

  val typeMap: Map[Int, (Maze, NodeData) => List[Directive]] = Map(6 -> turn(true), 8 -> turn(false))

  def turn(isSix: Boolean)(mazeData: Maze, indi: NodeData): List[Directive] = {
    val (direction1, direction2) = if (isSix) (LEFT, RIGHT) else (RIGHT, LEFT)
    if (indi._1 == 0) List(direction1)
    else if (indi._1 == w - 1) List(direction2)
    else if (List(2, 5, 6, 11).contains(mazeData(indi._2)(indi._1 - 1))) List(direction2)
    else List(direction1)
  }

  implicit def toDirective(command: String): Directive = directiveConverter(command)
  def inside(x: Int, y: Int): Boolean = y < h && y >= 0 && x < w && x >= 0
  def none(xy: Point): NodeData = (-1, -1, NONE)
  def left(xy: Point): NodeData = (xy._1 - 1, xy._2, RIGHT)
  def right(xy: Point): NodeData = (xy._1 + 1, xy._2, LEFT)
  def down(xy: Point): NodeData = (xy._1, xy._2 + 1, TOP)
  def up(xy: Point): NodeData = (xy._1, xy._2 - 1, NONE)

  class Graph(w: Int, h: Int, mazeData: Maze) {
    var marked = Set.empty[Node]

    private def selectLeftRight(commandList: List[Directive], nodeType: Int, data: NodeData) = {
      if (commandList.length == 2 && commandList.head == LEFT && commandList.tail.head == RIGHT) {
        typeMap(nodeType)(mazeData, data)
      } else commandList
    }

    private def getCommands(nodeType: Int, data: NodeData) = {
      if (nodeType < 0) List(WAIT) else
      turn(nodeType).get(data._3) match {
        case Some(commandList) => selectLeftRight(commandList, nodeType, data)
        case None => List.empty
      }
    }

    private def getCommandsList(node: Node, initXy: Point) = {
      var currentNode = node
      val out = new Array[String](node.step)
      val path = new Array[Point](node.step + 1)
      var stack = List.empty[String]
      path(0) = initXy

      while(currentNode.parent != null) {
        val arrIndex = currentNode.step - 1
        path(currentNode.step) = (currentNode.x, currentNode.y)

        if (currentNode.command == WAIT) {
          if (stack.nonEmpty) {
            out(arrIndex) = stack.head
            stack = stack.tail
          } else {
            out(arrIndex) = s"${currentNode.command}"
          }
        } else if (currentNode.command == DOUBLE) {
          val doubleCommand = s"${currentNode.x} ${currentNode.y} RIGHT"
          stack = doubleCommand :: stack
          out(arrIndex) = doubleCommand
        } else out(arrIndex) = s"${currentNode.x} ${currentNode.y} ${currentNode.command}"
        currentNode = currentNode.parent
      }
      (out, path)
    }


    def calculate(initX: Int, initY: Int, from: Directive, exitX: Int) = {
      var stack = List(Node(initX, initY, from, NONE, mazeData(initY)(initX), 0, 0, null))
      var exitNode: Option[Node] = None

      while(stack.nonEmpty) {
        val currentNode = stack.head
        marked = marked + currentNode
        if (currentNode.x == exitX && currentNode.y == h - 1) {
          stack = List.empty
          exitNode = Some(currentNode)
        } else {
          stack = stack.tail
          val xy = connection(Math.abs(currentNode.nodeType)).getOrElse(currentNode.from, none _)(currentNode.x, currentNode.y)
          if (inside(xy._1, xy._2)) {
            val newNodeType = mazeData(xy._2)(xy._1)
            val newNode = getCommands(newNodeType, xy).map { cmd =>
              Node(xy._1, xy._2, xy._3, cmd,
                if (newNodeType < 0) newNodeType else newType(newNodeType)(cmd),
                currentNode.step + 1,
                currentNode.newWaitIndex(cmd),
                currentNode)
            }.filterNot(node => marked.contains(node) || node.notLegalDouble)
            stack = newNode ::: stack
          }
        }
      }

      exitNode match {
        case Some(node) => getCommandsList(node, (initX, initY))
        case None => (Array.empty[String], Array.empty[Point])
      }
    }
  }

  val graph = new Graph(w, h, mazeData)
  var visitedSquares = Set.empty[Point]
  var commands: Array[String] = _
  var path: Array[Point] = _
  var disposed = Set.empty[Point]

  def calculateRock(rock: Rock, step: Int) {
    var x = rock.x
    var y = rock.y
    var from = rock.from
    var loop = true

    for (i <- step until path.length; if loop) {
      val indiXy = path(i)
      val rockType = mazeData(y)(x)
      if ((x, y) == indiXy) {
        loop = false
      } else {
        val newXy = connection(Math.abs(rockType)).getOrElse(from, none _)(x, y)
        if (inside(newXy._1, newXy._2)) {
          if (x != rock.x || y != rock.y) rock.path.addOne((x, y, rockType > 0))
          x = newXy._1
          y = newXy._2
          from = newXy._3
        } else loop = false
      }
    }
  }

  for (i <- LazyList.from(0).takeWhile(i => firstStep || i < path.length)) {
    val Array(_xi, _yi, posi) = readLine split " "
    val xi = _xi.toInt
    val yi = _yi.toInt

    if (firstStep) {
      val (_commands, _path) = graph.calculate(xi, yi, posi, exit)
      commands = _commands
      path = _path
      firstStep = false
    }

    visitedSquares = visitedSquares + ((xi, yi))
    val r = readLine.toInt
    var newPointRockMap = Map.empty[Point, Rock]

    val rocks = for (_ <- 0 until r) yield {
      val Array(_xr, _yr, posr) = readLine split " "
      val xr = _xr.toInt
      val yr = _yr.toInt
      val prevXy = prevSquare(posr)(xr, yr)
      val newRock = pointRockMap.get(prevXy._1, prevXy._2) match {
        case Some(rock) =>
          Rock(xr, yr, posr, if (rock.path.isEmpty) rock.path else rock.path.tail, rock.disposed || visitedSquares.contains((xr, yr)) || !rock.path.tail.exists(_._3))
        case _ =>
          val rock = Rock(xr, yr, posr, ListBuffer.empty, visitedSquares.contains((xr, yr)))
          if (!rock.disposed) calculateRock(rock, i)
          rock
      }
      newPointRockMap += (xr, yr) -> newRock
      newRock
    }
    pointRockMap = newPointRockMap
    newPointRockMap = null

    val cmd = if (commands(i) == WAIT_CMD) {
      val minRockSeq = rocks.filterNot(_.disposed).groupBy(_.stopListSize).minByOption(_._1).map(_._2)
      minRockSeq match {
        case Some(minRockList) =>
          val targetRock = minRockList.minBy(rock => rock.path.zipWithIndex.filter(_._1._3).head._2)
          val coord = targetRock.stopList.head
          pointRockMap += (targetRock.x, targetRock.y) -> Rock(targetRock.x, targetRock.y, targetRock.from, targetRock.path, true)
          s"${coord._1} ${coord._2} RIGHT"
        case None => WAIT_CMD
      }
    } else commands(i)
    println(cmd)
  }
}
