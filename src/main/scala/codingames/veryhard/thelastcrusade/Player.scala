 package codingames.veryhard.thelastcrusade

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.io.StdIn._

object Player extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
     val filename = "resources/thelastcrusade/AvoidingRocks.txt"
     val bufferedSource = Source.fromFile(filename)
     val fileData = bufferedSource.getLines
     def readInt = if (fileData.hasNext) fileData.next.toInt else -1
     def readLine = if (fileData.hasNext) fileData.next else "EOF"
//------------------------------------------FILE ENTRY------------------------------------------------------------------

  object Directive extends Enumeration {
    type Directive = Value
    val RIGHT, LEFT, TOP, WAIT, DOUBLE, NONE = Value
  }

  import Directive._

  type Point = (Int, Int)
//  type NodeData = (Int, Int, Directive)
  type RockPointData = (Int, Int, Boolean)
  type NextXY = Point => NodeData
  type Maze = IndexedSeq[Array[Int]]

  var i = 0
  var firstStep = true
  var newRockCame = false

  case class NodeData(x: Int, y: Int, direct: Directive) {
    def getPoint = (x, y)
  }
  case class Node(x: Int, y: Int, from: Directive, command: Directive, nodeType: Int, step: Int, waitIndex: Int, parent: Node) {
    lazy val notLegalDouble = waitIndex < 0 && command == DOUBLE

    def newWaitIndex(command: Directive) = if (command == WAIT) waitIndex + 1 else if (command == Directive.DOUBLE) waitIndex - 1 else waitIndex
  }

  class Rock(var x: Int, var y: Int, var from: Directive, var path: RockPath, var disposed: Boolean, var step: Int, var disposedPoint: Point) {

    def isDanger(currentStep: Int, indiPath: IndiPath) = {
      path.getClashByIndex(indiPath.id) match {
        case Some(clash) =>
          val stepNumber = currentStep + clash.clashStep - step
          stepNumber < indiPath.path.length && stepNumber >= 0 && indiPath.path(stepNumber) == clash.clashPoint
        case None => false
      }
    }

    def canAvoidClash(ip: IndiPath): Either[Boolean, Point] = {
      path.getClashByIndex(ip.id) match {
        case Some(clash) =>
          path.path.slice(step, clash.clashStep).tail.find(_._3).map(p => Right((p._1, p._2))).orElse(Some(Left(false))).get
        case None => Left(true)
      }
    }

    def canClashWithRock(clashCandidate: Rock): Boolean = {
      val clashCandidatePath = clashCandidate.path.path
      val actualClashCandidatePath = clashCandidatePath.slice(clashCandidate.step, clashCandidatePath.length)
      val myPath = path.path.slice(step, path.path.length)
      myPath.zipWithIndex.exists(aip => (aip._1._1, aip._1._2) == (actualClashCandidatePath(aip._2)._1, actualClashCandidatePath(aip._2)._1))
    }

    def setDisposedBy(p: Point) = /*Rock(x, y, from, path, true, step, p)*/{
      disposed = true
      disposedPoint = p
    }
    def stopList = path.stopList                                                             // TODO: calculate stop list depending on current rock position
    def stopListSize = path.stopListSize
    def tail = path.path.tail
    def isEmpty = path.path.isEmpty
    def zipWithIndex = path.path.zipWithIndex
  }

  class RockPath(val path: List[RockPointData], val clashData: Set[ClashData], var disposed: Boolean) {
    private lazy val clashDataMap = clashData.map(cd => (cd.indiPathIndex, cd)).toMap
    private lazy val clashDataIndices = clashDataMap.keySet
    lazy val stopList = path.filter(rpd => rpd._3 && !exists(rpd))                                                      // TODO: maybe need to recalculate more precisely
    lazy val stopListSize = stopList.length

    private def exists(rpd: RockPointData) = clashData.exists(cd => cd.clashPoint == (rpd._1, rpd._2))
    def closestPositive = path.find(_._3)
    def getClashByIndex(i: Int) = clashDataMap.get(i)
    def clashedWith(indiPathIndex: Int) = clashDataIndices.contains(indiPathIndex)
  }

  case class ClashData(indiPathIndex: Int, clashStep: Int, clashPoint: Point) {
    override def equals(obj: Any): Boolean = obj.isInstanceOf[ClashData] && obj.asInstanceOf[ClashData].indiPathIndex == indiPathIndex
    override def hashCode(): Int = indiPathIndex
  }

  case class IndiPath(id: Int, commands: Array[String], nodeData: Array[NodeData], dangerPath: ListBuffer[RockPath]) {
    lazy val path = nodeData.map(_.getPoint)
    lazy val pathMap = path.zipWithIndex.map(pi => ((pi._1._1, pi._1._2), pi._2)).toMap
    lazy val pathSet = pathMap.keySet
  }

  def none(xy: Point): NodeData = NodeData(-1, -1, NONE)

  def left(xy: Point): NodeData = NodeData(xy._1 - 1, xy._2, RIGHT)

  def right(xy: Point): NodeData = NodeData(xy._1 + 1, xy._2, LEFT)

  def down(xy: Point): NodeData = NodeData(xy._1, xy._2 + 1, TOP)

  def up(xy: Point): NodeData = NodeData(xy._1, xy._2 - 1, NONE)

  // w: number of columns.
  // h: number of rows.
  val Array(w, h) = (readLine split " ").map(_.toInt)
  Console.err.println(s"$w $h")
  val mazeData = for (_ <- 0 until h) yield readLine.split(" ").map(_.toInt)
  Console.err.println(s"${mazeData.map(_.mkString("\t")).mkString("\n")}")
  val exit = readLine.toInt // the coordinate along the X axis of the exit.
  Console.err.println(s"$exit")

  val graph = new Graph(w, h, mazeData)
  var visitedSquares = Set.empty[Point]
  var commands: Array[String] = _
  var allowedIndiPath: Array[IndiPath] = _
  var allRockPath: Array[RockPath] = _
  var rockPathMap = Map.empty[Point, RockPath]
  var currentIndiPath: IndiPath = _
  var currentIndiPathSet: Set[Point] = _
  var disposed = Set.empty[Point]

  var pointRockMap = Map.empty[Point, Rock]
  val directiveConverter = Map("LEFT" -> LEFT, "RIGHT" -> RIGHT, "TOP" -> TOP)
  val WAIT_CMD = "WAIT"

  implicit def toDirective(command: String): Directive = directiveConverter(command)
  def indiFilter(ind: Point, xi: Int, yi: Int) =   ind._1 != xi || ind._2 != yi

  val prevSquare: Map[Directive, NextXY] = Map(TOP -> up, LEFT -> left, RIGHT -> right)

  val connection: Array[Map[Directive, NextXY]] = Array(
    Map(), // 0
    Map(TOP -> down, RIGHT -> down, LEFT -> down), // 1
    Map(RIGHT -> left, LEFT -> right), // 2
    Map(TOP -> down), // 3
    Map(TOP -> left, RIGHT -> down), // 4
    Map(TOP -> right, LEFT -> down), // 5
    Map(RIGHT -> left, LEFT -> right), // 6
    Map(TOP -> down, RIGHT -> down), // 7
    Map(LEFT -> down, RIGHT -> down), // 8
    Map(TOP -> down, LEFT -> down), // 9
    Map(TOP -> left), // 10
    Map(TOP -> right), // 11
    Map(RIGHT -> down), // 12
    Map(LEFT -> down), // 13
  )

  val turn: Array[Map[Directive, List[Directive]]] = Array(
    Map(), // 0
    Map(TOP -> List(WAIT), RIGHT -> List(WAIT), LEFT -> List(WAIT)), // 1
    Map(TOP -> List(RIGHT), RIGHT -> List(WAIT), LEFT -> List(WAIT)), // 2
    Map(TOP -> List(WAIT), RIGHT -> List(RIGHT), LEFT -> List(LEFT)), // 3
    Map(TOP -> List(WAIT, LEFT), RIGHT -> List(WAIT), LEFT -> List(RIGHT)), // 4
    Map(TOP -> List(WAIT, RIGHT), RIGHT -> List(LEFT), LEFT -> List(WAIT)), // 5
    Map(TOP -> List(RIGHT, LEFT), RIGHT -> List(WAIT, RIGHT), LEFT -> List(WAIT, LEFT)), // 6
    Map(TOP -> List(WAIT), RIGHT -> List(WAIT, LEFT), LEFT -> List(RIGHT, LEFT)), // 7
    Map(TOP -> List(RIGHT, LEFT), RIGHT -> List(WAIT, DOUBLE), LEFT -> List(WAIT, DOUBLE)), // 8
    Map(TOP -> List(WAIT), RIGHT -> List(RIGHT, LEFT), LEFT -> List(WAIT, RIGHT)), // 9
    Map(TOP -> List(WAIT, RIGHT), LEFT -> List(LEFT), RIGHT -> List(DOUBLE)), // 10
    Map(TOP -> List(WAIT, LEFT), RIGHT -> List(RIGHT), LEFT -> List(DOUBLE)), // 11
    Map(TOP -> List(LEFT, DOUBLE), LEFT -> List(RIGHT), RIGHT -> List(WAIT)), // 12
    Map(TOP -> List(RIGHT, DOUBLE), LEFT -> List(WAIT), RIGHT -> List(LEFT)) // 13
  )

  val newType: Array[Map[Directive, Int]] = Array(
    Map(), // 0
    Map(RIGHT -> 1, LEFT -> 1, WAIT -> 1), // 1
    Map(RIGHT -> 3, LEFT -> 3, WAIT -> 2), // 2
    Map(RIGHT -> 2, LEFT -> 2, WAIT -> 3), // 3
    Map(RIGHT -> 5, LEFT -> 5, WAIT -> 4), // 4
    Map(RIGHT -> 4, LEFT -> 4, WAIT -> 5), // 5
    Map(RIGHT -> 7, LEFT -> 9, WAIT -> 6), // 6
    Map(RIGHT -> 8, LEFT -> 6, WAIT -> 7), // 7
    Map(RIGHT -> 9, LEFT -> 7, WAIT -> 8, DOUBLE -> -6), // 8
    Map(RIGHT -> 6, LEFT -> 8, WAIT -> 9), // 9
    Map(RIGHT -> 11, LEFT -> 13, WAIT -> 10, DOUBLE -> 12), // 10
    Map(RIGHT -> 12, LEFT -> 10, WAIT -> 11, DOUBLE -> 13), // 11
    Map(RIGHT -> 13, LEFT -> 11, WAIT -> 12, DOUBLE -> 10), // 12
    Map(RIGHT -> 10, LEFT -> 12, WAIT -> 13, DOUBLE -> 11) // 13
  )

  def turn(isSix: Boolean)(mazeData: Maze, indi: NodeData): List[Directive] = {
    val (direction1, direction2) = if (isSix) (RIGHT, LEFT) else (RIGHT, LEFT)
    if (indi.x == 0) List(direction1)
    else if (indi.x == w - 1) List(direction2)
    else if (List(2, 5, 6, 11).contains(mazeData(indi.y)(indi.x - 1))) List(direction2)
    else List(direction1)
  }

  val typeMap: Map[Int, (Maze, NodeData) => List[Directive]] = Map(6 -> turn(true), 8 -> turn(false))

  def inside(x: Int, y: Int): Boolean = y < h && y >= 0 && x < w && x >= 0

  def findSuspicious() = WAIT_CMD

  def findRocksPath(xi: Int, yi: Int) = {
    val firstRow = mazeData(0)
    val firstCol = mazeData.map(row => row(0))
    val lastCol = mazeData.map(row => row(firstRow.length - 1))

    val firstRowIndices = firstRow.zipWithIndex.withFilter(_._1 != 0).map(a => (a._2, 0))
    val firstColIndices = firstCol.zipWithIndex.withFilter(_._1 != 0).map(a => (0, a._2)).toArray
    val lastColIndices = lastCol.zipWithIndex.withFilter(_._1 != 0).map(a => (firstRow.length - 1, a._2)).toArray

    (firstRowIndices.withFilter(indiFilter(_, xi, yi)).map(firstRowIndex => calculateRockPath(firstRowIndex._1, firstRowIndex._2, TOP)) ++
     firstColIndices.withFilter(indiFilter(_, xi, yi)).map(firstRowIndex => calculateRockPath(firstRowIndex._1, firstRowIndex._2, LEFT)) ++
     lastColIndices.withFilter(indiFilter(_, xi, yi)).map(firstRowIndex => calculateRockPath(firstRowIndex._1, firstRowIndex._2, RIGHT))).filter(_.clashData.nonEmpty)
        .map(rp => ((rp.path(0)._1, rp.path(0)._2), rp)).toMap
  }

  def calculateRockPath(xr: Int, yr: Int, posr: Directive) = {
    var x = xr
    var y = yr
    var from = posr
    var loop = true
    var step = 0
    val indiPathId = mutable.Set.empty[Int]
    val rockPath = ListBuffer((xr, yr, false))
    val clashData = mutable.Set.empty[ClashData]

    while (loop) {
      val rockType = mazeData(y)(x)
      val newXy = connection(Math.abs(rockType)).getOrElse(from, none _)(x, y)
      if (inside(newXy.x, newXy.y)) {
        allowedIndiPath.indices.foreach { indiPathIndex =>
          if (allowedIndiPath(indiPathIndex).pathSet((x, y))) {
            clashData.addOne(ClashData(indiPathIndex, step, (x, y)))
            indiPathId.addOne(indiPathIndex)
          }
        }

        if (x != xr || y != yr) rockPath.addOne((x, y, rockType > 0))
        x = newXy.x
        y = newXy.y
        from = newXy.direct
        step += 1
      } else loop = false
    }
    new RockPath(rockPath.toList, clashData.toSet, false)
  }


  class Graph(w: Int, h: Int, mazeData: Maze) {
    var marked = Set.empty[Node]
    val pathList = ListBuffer.empty[Node]

    private def selectLeftRight(commandList: List[Directive], nodeType: Int, data: NodeData) = {
      if (commandList.length == 2 && commandList.head == LEFT && commandList.tail.head == RIGHT) {
        typeMap.get(nodeType) match {
          case Some(res) => res(mazeData, data)
          case None => commandList
        }
      } else commandList
    }

    def getCommands(nodeType: Int, data: NodeData) = {
      if (nodeType < 0) List(WAIT) else
        turn(nodeType).get(data.direct) match {
          case Some(commandList) => selectLeftRight(commandList, nodeType, data)
          case None => List.empty
        }
    }

    def getIndiPath(nodeI: (Node, Int), initXy: Point) = {
      var currentNode = nodeI._1
      val commands = new Array[String](currentNode.step)
      val nodeData = new Array[NodeData](currentNode.step + 1)
      var stack = List.empty[String]
      nodeData(0) = NodeData(initXy._1, initXy._2, NONE)

      while(currentNode.parent != null) {
        val arrIndex = currentNode.step - 1
        nodeData(currentNode.step) = NodeData(currentNode.x, currentNode.y, currentNode.from)

        if (currentNode.command == WAIT) {
          if (stack.nonEmpty) {
            commands(arrIndex) = stack.head
            stack = stack.tail
          } else {
            commands(arrIndex) = s"${currentNode.command}"
          }
        } else if (currentNode.command == DOUBLE) {
          val doubleCommand = s"${currentNode.x} ${currentNode.y} RIGHT"
          stack = doubleCommand :: stack
          commands(arrIndex) = doubleCommand
        } else commands(arrIndex) = s"${currentNode.x} ${currentNode.y} ${currentNode.command}"
        currentNode = currentNode.parent
      }
      IndiPath(nodeI._2, commands, nodeData, ListBuffer.empty[RockPath])
    }


    def calculate(initX: Int, initY: Int, from: Directive, exitX: Int) = {
      var stack = List(Node(initX, initY, from, NONE, mazeData(initY)(initX), 0, 0, null))
      var exitNodes = ListBuffer.empty[Node]

      while(stack.nonEmpty) {
        val currentNode = stack.head
        marked = marked + currentNode
        if (currentNode.x == exitX && currentNode.y == h - 1) {
          stack = stack.tail
          pathList.addOne(currentNode)
          exitNodes.addOne(currentNode)
        } else {
          stack = stack.tail
          val xy = connection(Math.abs(currentNode.nodeType)).getOrElse(currentNode.from, none _)(currentNode.x, currentNode.y)
          if (inside(xy.x, xy.y)) {
            val newNodeType = mazeData(xy.y)(xy.x)
            val newNode = getCommands(newNodeType, xy).map { cmd =>
              Node(xy.x, xy.y, xy.direct, cmd,
                if (newNodeType < 0) newNodeType else newType(newNodeType)(cmd),
                currentNode.step + 1,
                currentNode.newWaitIndex(cmd),
                currentNode)
            }.filterNot(node => marked.contains(node) || node.notLegalDouble)
            stack = newNode ::: stack
          }
        }
      }

      val paths = exitNodes.zipWithIndex.map(getIndiPath(_, (initX, initY)))
      paths.toArray
    }
  }

  def findAnotherPath(currentIndiPath: IndiPath, rock: Rock, step: Int) = {
    val safePaths = allowedIndiPath.filterNot(indiPath => indiPath.id == currentIndiPath.id || rock.isDanger(step, indiPath))
    if (safePaths.isEmpty) {
      Console.err.println(s"IT SHOULD NOT HAPPEN!")
      None
    } else Some(safePaths.head)
  }

  def findMostDangerRockPath() = {
    val rockPathsCanBeSafe = allRockPath.filterNot(path => path.stopListSize == 0 || path.disposed)
    if (rockPathsCanBeSafe.nonEmpty) {
      val minRockPath = rockPathsCanBeSafe.minBy(path => path.stopListSize)
      minRockPath.disposed = true
      minRockPath.closestPositive
    } else None
  }

  def tryToClashRocks(rocks: IndexedSeq[Rock], dangerRock: Rock): Option[Rock] = {
    val candidates = rocks.filterNot(_ == dangerRock)
    candidates.find(rock => rock.canClashWithRock(dangerRock))
  }

  def tryToCorrect(expectedNext: Point, squareType: Int, affectedPoint: NodeData, from: Directive) = {
    List(LEFT, RIGHT, WAIT).filter(_ != affectedPoint.direct).find((command: Directive) => {
      val newSquareType = newType(squareType)(command)
      connection(newSquareType).get(from).map(_ (affectedPoint.getPoint).getPoint).contains(expectedNext)
    }).map(v => {
      if (v == WAIT) WAIT_CMD
      else s"${affectedPoint.x} ${affectedPoint.y} $v"
    }).getOrElse(currentIndiPath.commands(i))
  }

  def correct(affectedPoint: NodeData, cmd: String): String = {
    currentIndiPath.pathMap.get((affectedPoint.x, affectedPoint.y)).map(affectedPointIndex => {
      val expectedNext = currentIndiPath.path(affectedPointIndex + 1)
      val affectedPointFrom = currentIndiPath.nodeData(affectedPointIndex).direct
      val squareType = mazeData(affectedPoint.y)(affectedPoint.x)
      val nextSquareType = newType(squareType)(affectedPoint.direct)
      val realNext = connection(nextSquareType)
        .get(affectedPointFrom)
        .map(_(affectedPoint.getPoint).getPoint)
        .getOrElse(expectedNext)
      if (expectedNext != realNext) tryToCorrect(expectedNext, squareType, affectedPoint, affectedPointFrom)
      else cmd
    }).getOrElse(cmd)
  }

  def commandToNodeData(command: String): Either[String, NodeData] = {
    val currentCommandArr = command.split(" ")
    val currentDirective = currentCommandArr(currentCommandArr.length - 1)
    if (currentDirective == WAIT_CMD) Left(WAIT_CMD) else
      Right(NodeData(
        Integer.parseInt(currentCommandArr(0)),
        Integer.parseInt(currentCommandArr(1)),
        currentDirective ))
  }

  for (i <- LazyList.from(0).takeWhile(i => firstStep || i < currentIndiPath.path.length)) {
    val Array(_xi, _yi, posi) = readLine split " "
    val xi = _xi.toInt
    val yi = _yi.toInt
    Console.err.println(s"x=$xi y=$yi $posi") // Indiana position and where he came from

    if (firstStep) {
      allowedIndiPath = graph.calculate(xi, yi, posi, exit)
      rockPathMap = findRocksPath(xi, yi)
      allRockPath = rockPathMap.values.toArray
      for (i <- allowedIndiPath.indices; rockPath <- allRockPath; if rockPath.clashedWith(i))
        allowedIndiPath(i).dangerPath.addOne(rockPath)

      currentIndiPath = allowedIndiPath.minBy(_.dangerPath.length)
      firstStep = false
    }

    allowedIndiPath = allowedIndiPath.filter(indiPath => indiPath.path(i + 1) == currentIndiPath.path(i + 1))

    visitedSquares = visitedSquares + ((xi, yi))
    val r = readLine.toInt                                                                                              // the number of rocks currently in the grid.
    Console.err.println(s"rocks=$r")

    var newPointRockMap = Map.empty[Point, Rock]

    val rocks = for (_ <- 0 until r) yield {
      val Array(_xr, _yr, posr) = readLine split " "
      val xr = _xr.toInt
      val yr = _yr.toInt
      Console.err.println(s"$xr $yr $posr")
      val prevXy = prevSquare(posr)(xr, yr)
      val newRock = pointRockMap.get(prevXy.x, prevXy.y) match {
        case Some(rock) => // Rock(xr, yr, posr, rock.path, rock.disposed, rock.step + 1, rock.disposedPoint)
          rock.x = xr
          rock.y = yr
          rock.from = posr
          rock.step = rock.step + 1
          rock
        case None => rockPathMap.get((xr, yr)) match {
          case Some(rockPath) => new Rock(xr, yr, posr, rockPath, false, 0, null)
          case None => new Rock(xr, yr, posr, null, true, 0, null)                                                        // a rock arose in unexpected point. Never should happen
        }
      }
      newPointRockMap += (xr, yr) -> newRock
      newRock
    }
    val aliveRocks = rocks.filterNot(_.disposed)
    pointRockMap = newPointRockMap
    newPointRockMap = null

    val cmd: String = if (currentIndiPath.commands(i) == WAIT_CMD) {
      val dangerRock = aliveRocks.find(r => r.isDanger(i, currentIndiPath))
      dangerRock match {
        case Some(rock) =>
          rock.canAvoidClash(currentIndiPath) match {
            case Right(p) =>
              rock.setDisposedBy(p)
              s"${p._1} ${p._2} RIGHT"
            case Left(nothingToDo) => if (nothingToDo) currentIndiPath.commands(i) else {
                allowedIndiPath = allowedIndiPath.filterNot(_.id == currentIndiPath.id)
                findAnotherPath(currentIndiPath, rock, i) match {
                  case Some(path) =>
                    currentIndiPath = path
                    currentIndiPath.commands(i)
                  case None => tryToClashRocks(rocks, rock).map(redisposedRock => if (redisposedRock.disposedPoint == null) currentIndiPath.commands(i) else {
                        val out = s"${redisposedRock.disposedPoint._1} ${redisposedRock.disposedPoint._2} LEFT"
                        redisposedRock.setDisposedBy(null)
                        rock.setDisposedBy(null)
                        out
                      }
                    ).getOrElse(currentIndiPath.commands(i))
                }
              }
          }
        case None => findMostDangerRockPath().map(d => s"${d._1} ${d._2} LEFT").getOrElse(currentIndiPath.commands(i))
      }
    } else currentIndiPath.commands(i)


    val correctedCmd = commandToNodeData(cmd).map(correct(_, cmd)).getOrElse(cmd)

    commandToNodeData(correctedCmd).foreach(n => mazeData(n.y)(n.x) = newType(mazeData(n.y)(n.x))(n.direct))
    println(correctedCmd)
  }
}
