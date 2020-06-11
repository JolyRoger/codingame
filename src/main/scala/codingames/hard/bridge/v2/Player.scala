package codingames.hard.bridge.v2

import scala.io.StdIn._

object Player extends App {
  type Point = Array[Int]
  type StackItem = (Int, List[Point])
  type Stack = List[StackItem]
  type MotoData = (Point, List[Point])

  val m = readInt // the amount of motorbikes to control
  val v = readInt // the minimum amount of motorbikes that must survive
  val l0 = readLine // L0 to L3 are lanes of the road. A dot character . represents a safe space, a zero 0 represents a hole in the road.
  val l1 = readLine
  val l2 = readLine
  val l3 = readLine

  val order = Map(0 -> "SPEED", 1 -> "JUMP", 2 -> "WAIT", 3 -> "UP", 4 -> "DOWN", 5 -> "SLOW")

  val commands = Map(0 -> ((x: Int, y: Int, a: Int, s: Int) => {
    (Array(x + s + 1, y, a, s + 1), (x to x + s + 1).map(Array(_, y)).toList)
  }), 5 -> ((x: Int, y: Int, a: Int, s: Int) => {
    (Array(x + s - 1, y, a, s - 1), (x until x + s).map(Array(_, y)).toList)
  }), 1 -> ((x: Int, y: Int, a: Int, s: Int) => {
    (Array(x + s, y, a, s), List(Array(x, y), Array(x + s, y)))
  }), 2 -> ((x: Int, y: Int, a: Int, s: Int) => {
    (Array(x + s, y, a, s), (x to x + s).map(Array(_, y)).toList)
  }), 3 -> ((x: Int, y: Int, a: Int, s: Int) => {
    (Array(x + s, y - 1, a, s), if (s == 0) List(Array(x, y), Array(x, y - 1)) else ((x until x + s).map(Array(_, y)) :: (x + 1 to x + s).map(Array(_, y - 1)) :: Nil).flatten)
  }), 4 -> ((x: Int, y: Int, a: Int, s: Int) => {
    (Array(x + s, y + 1, a, s), if (s == 0) List(Array(x, y), Array(x, y + 1)) else ((x until x + s).map(Array(_, y)) :: (x + 1 to x + s).map(Array(_, y + 1)) :: Nil).flatten)
  }))

  val lines = Array(l0, l1, l2, l3)
  val roadLength = lines(0).length
  var step = 0

  def string(lst: List[Array[Int]]) = lst.map(_.mkString("", "", "")).reduce(_ + ":" + _)

  def newXYS(next: StackItem) = {
    val (c, pathData) = next
    pathData.map(pd => survive(commands(c)(pd(0), pd(1), pd(2), pd(3))))
  }

  def isActive(stack: Stack) = stack.isEmpty || (stack.size < 50 && stack.head._2.head(0) < roadLength)

  def notContain(pointSet: Set[String], item: List[Point]) = !pointSet.contains(string(item))

  def can(liveMoto: List[MotoData], oldXYS: List[Point], usedSquares: Set[String], mustSurvive: Int) = {
    liveMoto.size >= mustSurvive &&
          liveMoto.zipWithIndex.forall(motoWithIndex => {
            val (moto, index) = motoWithIndex
            moto._1(1) >= 0 && moto._1(1) < lines.length &&
            (moto._1(0) != oldXYS(index)(0) || moto._1(1) != oldXYS(index)(1) || moto._1(3) != oldXYS(index)(3)) &&
              !usedSquares.contains(string(liveMoto.map(_._1)))
          })
  }

  def update(stack: Stack): Stack = {
    var stackHead = stack.head
    var outStack = stack.tail
    var command = stackHead._1 + 1

    while (command > order.keys.max) {
      if (outStack.isEmpty) return outStack // can't find a way
      stackHead = outStack.head
      outStack = outStack.tail
      command = stackHead._1 + 1
    }
    (command, stackHead._2) :: outStack
  }

  def survive(moto: MotoData) = {
    (if (moto._2.forall(point => {
      point(0) < 0 || point(0) >= roadLength || point(1) < 0 || point(1) >= lines.length || lines(point(1))(point(0)) == '.'
    })) moto._1 else Array(moto._1(0), moto._1(1), 0, moto._1(3)), moto._2)
  }

  def successful(stackItem: StackItem) = stackItem._2.exists(_(0) >= roadLength)

  def calc(init: List[Point], mustSurvive: Int): Stack = {
    var stack = (0, init) :: Nil
    var usedSquares = Set.empty[String]

    do {
      if (stack.isEmpty) return stack
      val newCoords = newXYS(stack.head).filter(_._1(2) == 1)
      stack = if (can(newCoords, stack.head._2, usedSquares, mustSurvive)) {
        val ncm = newCoords.map(_._1)
        usedSquares += string(ncm)
        (0, ncm) :: stack
      } else update(stack)
    } while (isActive(stack))
    stack
  }.reverse

  var instructions: Stack = List.empty

  while (true) {
    val s = readInt
    val moto = (for (i <- 0 until m) yield ((readLine + s" $s") split " ").map(_.toInt)).toList
    val stack = (v to m).map(calc(moto, _)).reverse.find(item => item.nonEmpty && successful(item.last))
    if (step == 0) instructions = stack.get

    println(order(instructions(step)._1))
    step = step + 1
  }
}
