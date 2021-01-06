package codingames.hard.thelastcrusade

import java.util.Objects

import scala.collection.immutable.ListSet
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.io.StdIn._


object Player extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
  val filename = "resources/thelastcrusade/Rocks1.txt"
  val bufferedSource = Source.fromFile(filename)
  val fileData = bufferedSource.getLines
  def readInt = if (fileData.hasNext) fileData.next.toInt else -1
  def readLine = if (fileData.hasNext) fileData.next else "EOF"
//------------------------------------------FILE ENTRY------------------------------------------------------------------

  object Directive extends Enumeration {
    type Directive = Value
    val LEFT, RIGHT, TOP, WAIT, DOUBLE, NONE = Value
  }

  import Directive._

  type Point = (Int, Int)
  type NextXY = (Int, Int) => (Int, Int, Directive)

  var i = 0
  var firstStep = true
  val enter = Array(List("2", "1", RIGHT), List(WAIT), List(WAIT))

  case class NodeData(x: Int, y: Int, from: Directive, command: Directive, nodeType: Int, step: Int, waitIndex: Int, parent: NodeData) {
    lazy val notLegalDouble = waitIndex < 0 && command == DOUBLE

    def newWaitIndex(command: Directive) = if (command == WAIT) waitIndex + 1 else if (command == Directive.DOUBLE) waitIndex - 1 else waitIndex

    override def hashCode(): Int = Objects.hashCode((x, y, from, command));

    override def equals(obj: Any): Boolean = {
      val nd = obj.asInstanceOf[NodeData]
      x == nd.x && y == nd.y && from == nd.from && command == nd.command
    }
  }

  case class Rock(x: Int, y: Int, from: Directive, path: ListBuffer[Point])

  def none(x: Int, y: Int) = (-1, -1, NONE)
  def left(x: Int, y: Int) = (x - 1, y, RIGHT)
  def right(x: Int, y: Int) = (x + 1, y, LEFT)
  def down(x: Int, y: Int) = (x, y + 1, TOP)
  def up(x: Int, y: Int) = (x, y - 1, NONE)

  // w: number of columns.
  // h: number of rows.
  val Array(w, h) = (readLine split " ").map(_.toInt)
  Console.err.println(s"$w $h")
  val mazeData = for (_ <- 0 until h) yield readLine.split(" ").map(_.toInt)
  Console.err.println(s"${mazeData.map(_.mkString("\t")).mkString("\n")}")
  val exit = readLine.toInt // the coordinate along the X axis of the exit.
  Console.err.println(s"$exit")

  var pointRockMap = Map.empty[Point, Rock]
  val directiveConverter = Map("LEFT" -> LEFT, "RIGHT" -> RIGHT, "TOP" -> TOP)

  implicit def toString(d: Directive): String = d.toString
  implicit def toDirective(command: String): Directive = directiveConverter(command)

  val prevSquare: Map[Directive, NextXY] = Map(TOP -> up, LEFT -> left, RIGHT -> right)

  val connection: Array[Map[Directive, NextXY]] = Array(
    Map(),                                              // 0
    Map(TOP -> down, RIGHT -> down, LEFT -> down),      // 1
    Map(RIGHT -> left, LEFT -> right),                  // 2
    Map(TOP -> down),                                   // 3
    Map(TOP -> left, RIGHT -> down),                    // 4
    Map(TOP -> right, LEFT -> down),                    // 5
    Map(RIGHT -> left, LEFT -> right),                  // 6
    Map(TOP -> down, RIGHT -> down),                    // 7
    Map(LEFT -> down, RIGHT -> down),                   // 8
    Map(TOP -> down, LEFT -> down),                     // 9
    Map(TOP -> left),                                   // 10
    Map(TOP -> right),                                  // 11
    Map(RIGHT -> down),                                 // 12
    Map(LEFT -> down),                                  // 13
  )

  val turn: Array[Map[Directive, List[Directive]]] = Array(
    Map(),                                                                                    // 0
    Map(TOP -> List(WAIT), RIGHT -> List(WAIT), LEFT -> List(WAIT)),                          // 1
    Map(TOP -> List(RIGHT), RIGHT -> List(WAIT), LEFT -> List(WAIT)),                         // 2
    Map(TOP -> List(WAIT), RIGHT -> List(RIGHT), LEFT -> List(LEFT)),                         // 3
    Map(TOP -> List(WAIT, LEFT), RIGHT -> List(WAIT), LEFT -> List(RIGHT)),                   // 4
    Map(TOP -> List(WAIT, RIGHT), RIGHT -> List(LEFT), LEFT -> List(WAIT)),                   // 5
    Map(TOP -> List(LEFT, RIGHT), RIGHT -> List(WAIT, RIGHT), LEFT -> List(WAIT, LEFT)),      // 6
    Map(TOP -> List(WAIT), RIGHT -> List(LEFT, RIGHT), LEFT -> List(LEFT, RIGHT)),            // 7
    Map(TOP -> List(LEFT, RIGHT), RIGHT -> List(WAIT, DOUBLE), LEFT -> List(WAIT, DOUBLE)),   // 8
    Map(TOP -> List(WAIT), RIGHT -> List(LEFT, RIGHT), LEFT -> List(WAIT, RIGHT)),            // 9
    Map(TOP -> List(WAIT, RIGHT), LEFT -> List(LEFT), RIGHT -> List(DOUBLE)),                 // 10
    Map(TOP -> List(WAIT, LEFT), RIGHT -> List(RIGHT), LEFT -> List(DOUBLE)),                 // 11
    Map(TOP -> List(LEFT, DOUBLE), LEFT -> List(RIGHT), RIGHT -> List(WAIT)),                 // 12
    Map(TOP -> List(RIGHT, DOUBLE), LEFT -> List(WAIT), RIGHT -> List(LEFT))                  // 13
  )

  val newType: Array[Map[Directive, Int]] = Array(
    Map(),                                                      // 0
    Map(RIGHT -> -1,  LEFT -> -1 , WAIT -> -1),                 // 1
    Map(RIGHT -> -3,  LEFT -> -3 , WAIT -> -2),                 // 2
    Map(RIGHT -> -2,  LEFT -> -2 , WAIT -> -3),                 // 3
    Map(RIGHT -> -5,  LEFT -> -5 , WAIT -> -4),                 // 4
    Map(RIGHT -> -4,  LEFT -> -4 , WAIT -> -5),                 // 5
    Map(RIGHT -> -7,  LEFT -> -9 , WAIT -> -6),                 // 6
    Map(RIGHT -> -8,  LEFT -> -6 , WAIT -> -7),                 // 7
    Map(RIGHT -> -9,  LEFT -> -7 , WAIT -> -8, DOUBLE -> -6),   // 8
    Map(RIGHT -> -6,  LEFT -> -8 , WAIT -> -9),                 // 9
    Map(RIGHT -> -11, LEFT -> -13, WAIT -> -10, DOUBLE -> -12), // 10
    Map(RIGHT -> -12, LEFT -> -10, WAIT -> -11, DOUBLE -> -13), // 11
    Map(RIGHT -> -13, LEFT -> -11, WAIT -> -12, DOUBLE -> -10), // 12
    Map(RIGHT -> -10, LEFT -> -12, WAIT -> -13, DOUBLE -> -11)  // 13
  )

  def inside(x: Int, y: Int): Boolean = y < h && y >= 0 && x < w && x >= 0
//  def inside(xy: Point): Boolean = xy._2 < h && xy._2 >= 0 && xy._1 < w && xy._1 >= 0

  class Graph(w: Int, h: Int, mazeData: IndexedSeq[Array[Int]]) {
    var marked = Set.empty[NodeData]

    def getCommands(nodeType: Int, from: Directive) = {
      if (nodeType < 0) List(WAIT) else
      turn(nodeType).getOrElse(from, List.empty)
    }

    def getCommandsList(node: NodeData, initXy: Point): (Array[String], Array[Point]) = {
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
      var stack = List(NodeData(initX, initY, from, NONE, mazeData(initY)(initX), 0, 0, null))
      var exitNode: Option[NodeData] = None

      while(stack.nonEmpty) {
        val currentNode = stack.head
        marked = marked + currentNode
//        Console.err.println(s"${currentNode.x } ${currentNode.y} ${currentNode.command}\t[${currentNode.from}]")
        if (currentNode.x == exitX && currentNode.y == h - 1) {
          stack = List.empty
          exitNode = Some(currentNode)
        } else {
          stack = stack.tail
          val xy = connection(Math.abs(currentNode.nodeType)).getOrElse(currentNode.from, none _)(currentNode.x, currentNode.y)
          if (inside(xy._1, xy._2)) {
            val newNodeType = mazeData(xy._2)(xy._1)
            val newNode = getCommands(newNodeType, xy._3).map { cmd =>
              NodeData(xy._1, xy._2, xy._3, cmd,
                if (newNodeType < 0) newNodeType else newType(newNodeType)(cmd),
                currentNode.step + 1,
                currentNode.newWaitIndex(cmd),
                currentNode)
            }.filterNot(node => marked.contains(node) || node.notLegalDouble)
            stack = newNode ::: stack
          }
//          Console.err.println(s"${newNode.mkString("\n\t")} ")
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

//  lazy val (commands, path) = (x: Int, y: Int, pos: Directive, ex: Int) => graph.calculate(x, y, pos, ex)
  var commands: Array[String] = _
  var path: Array[Point] = _
  var stopRock = ListSet.empty[Point]
  var disposed = Set.empty[Point]

  def calculateRock(rock: Rock, step: Int) {
    var x = rock.x
    var y = rock.y
    var from = rock.from
    var loop = true
    var stepToClash = Int.MaxValue

    for (i <- step until path.length; if loop) {
      val indiXy = path(i)
      if ((x, y) == indiXy) {
        stepToClash = i
        rock.path.addOne((x, y))
        loop = false
      } else {
        val rockType = mazeData(y)(x)
        if (rockType > 0 && (rock.x != x || rock.y != y) && !disposed.contains((x, y))) {
          disposed = disposed + ((x, y))
          stopRock = stopRock + ((x, y))
        }
        val newXy = connection(Math.abs(rockType)).getOrElse(from, none _)(x, y)
        if (inside(newXy._1, newXy._2)) {
          rock.path.addOne((x, y))
          x = newXy._1
          y = newXy._2
          from = newXy._3
        } else loop = false
      }
    }
  }


  //  for (i <- LazyList.from(0).takeWhile(_ => currentNode.parent != null)) {
  for (i <- LazyList.from(0).takeWhile(i => firstStep || i < path.length)) {
    val Array(_xi, _yi, posi) = readLine split " "
    val xi = _xi.toInt
    val yi = _yi.toInt
    Console.err.println(s"x=$xi y=$yi $posi") // Indiana position and where he came from
    visitedSquares = visitedSquares + ((xi, yi))
    val r = readLine.toInt // the number of rocks currently in the grid.
    Console.err.println(s"rocks=$r")
    val rocks = for (_ <- 0 until r) yield {
      val Array(_xr, _yr, posr) = readLine split " "
      val xr = _xr.toInt
      val yr = _yr.toInt
      Console.err.println(s"$xr $yr $posr")
      val prevXy = prevSquare(posr)(xr, yr)
      disposed = disposed + ((xr, yr))
      stopRock -= ((xr, yr))
      val newRock = pointRockMap.get(prevXy._1, prevXy._2) match {
        case Some(rock) =>
          pointRockMap = pointRockMap - ((prevXy._1, prevXy._2))
          Rock(xr, yr, posr, rock.path)
        case _ => Rock(xr, yr, posr, ListBuffer.empty[Point])
      }
      pointRockMap += (xr, yr) -> newRock
      newRock
    }

    val dangRocks = rocks.filterNot(rock => visitedSquares.contains(rock.x, rock.y))

    dangRocks.foreach(rock => calculateRock(rock, i))

    Console.err.println(s"-----------------$dangRocks")

    if (firstStep) {
      val (_commands, _path) = graph.calculate(xi, yi, posi, exit)
      commands = _commands
      path = _path
      firstStep = false
    }

    val cmd = if (commands(i) == "WAIT" && stopRock.nonEmpty) {
      val coord = stopRock.head
      stopRock = stopRock.tail
      s"${coord._1} ${coord._2} RIGHT"
    } else commands(i)

    println(cmd)
  }
}
