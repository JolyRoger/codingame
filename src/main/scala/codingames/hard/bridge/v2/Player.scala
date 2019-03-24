package codingames.hard.bridge.v2

object Player extends App {
  type Point = (Int, Int, Int)
  type PathData = (Int, Point)
  type MotoData = (Int, Int, Int, List[(Int, Int)])
  type Rule = Point => MotoData
  type Command = Map[String, Rule]

  val m = readInt // the amount of motorbikes to control
  val v = readInt // the minimum amount of motorbikes that must survive
  val l0 = readLine // L0 to L3 are lanes of the road. A dot character . represents a safe space, a zero 0 represents a hole in the road.
  val l1 = readLine
  val l2 = readLine
  val l3 = readLine

  /*val*/ def order = Map(0 -> "SPEED", 1 -> "JUMP", 2 -> "WAIT", 3 -> "UP", 4 -> "DOWN", 5 -> "SLOW")

  /*val*/ def commands = Map(0 -> ((x: Int, y: Int, s: Int) => {
    (x + s + 1, y, s + 1, (x to x + s + 1).map((_, y)).toList)
  }), 5 -> ((x: Int, y: Int, s: Int) => {
    (x + s - 1, y, s - 1, (x until x + s).map((_, y)).toList)
  }), 1 -> ((x: Int, y: Int, s: Int) => {
    (x + s, y, s, List((x, y), (x + s, y)))
  }), 2 -> ((x: Int, y: Int, s: Int) => {
    (x + s, y, s, (x to x + s).map((_, y)).toList)
  }), 3 -> ((x: Int, y: Int, s: Int) => {
    (x + s, y - 1, s, if (s == 0) List((x, y), (x, y - 1)) else ((x until x + s).map((_, y)) :: (x + 1 to x + s).map((_, y - 1)) :: Nil).flatten)
  }), 4 -> ((x: Int, y: Int, s: Int) => {
    (x + s, y + 1, s, if (s == 0) List((x, y), (x, y + 1)) else ((x until x + s).map((_, y)) :: (x + 1 to x + s).map((_, y + 1)) :: Nil).flatten)
  }))

  var usedSquares = Set.empty[Point]

  val lines = Array(l0, l1, l2, l3)

  Console.err.println(s"LINES")
  printLines(lines.map(_.toCharArray))
  Console.err.println

  /*val*/ def roadLength = lines(0).length

  var motolines = lines.map(_.toCharArray)
  Console.err.println(s"the amount of motorbikes to control: $m")
  Console.err.println(s"the minimum amount of motorbikes that must survive: $v")

  def printLines(lines: Array[Array[Char]]) = lines.foreach(line => Console.err.println(s"lanes of the road: ${line.mkString("", "", "")}"))

  var step = 0


  def newXYS(stack: List[PathData]) = {
    val (c, (x, y, s)) = stack.head
    commands(c)(x, y, s)
  }

  def isActive(stack: List[PathData]) = stack.size < 50 && stack.head._2._1 < roadLength

  def can(data: MotoData, oldXYS: (Int, Int, Int)) = data._4.forall(xy => {
      val (sym, line) = xy
      line >= 0 && line < 4 &&
      sym >= 0 && (if (sym < roadLength) lines(line)(sym) == '.' else true) &&
      (data._1 != oldXYS._1 || data._2 != oldXYS._2 || data._3 != oldXYS._3) && !usedSquares.contains(data._1, data._2, data._3)
    })

  def update(stack: List[PathData]) = {
    var stackHead = stack.head
    var outStack = stack.tail
    var command = stackHead._1 + 1

    while (command > 5) {
      stackHead = outStack.head
      outStack = outStack.tail
      command = stackHead._1 + 1
    }
    (command, stackHead._2) :: outStack
  }

  def calc(initX: Int, initY: Int, initS: Int) = {
    Console.err.println(s"init: $initX $initY $initS")
    var stack = (0, (initX, initY, initS)) :: Nil
    do {
      val (newX, newY, newS, points) = newXYS(stack)
      stack = if (can((newX, newY, newS, points), stack.head._2)) {
        usedSquares += ((newX, newY, newS))
        (0, (newX, newY, newS)) :: stack
      } else update(stack)
    } while (isActive(stack))
    stack
  }.reverse

  var instructions: List[PathData] = List.empty

  while (true) {
    val s = readInt
    for (i <- 0 until m) {
      val Array(x, y, a) = for (j <- readLine split " ") yield j.toInt
      if (step == 0) instructions = calc(x, y, s)
      if (x < motolines(y).length) motolines(y)(x) = '*'
//      Console.err.println(s"x[$i]=$x y[$i]=$y s[$i]=$s")
    }

//    printLines(motolines)
    println(order(instructions(step)._1))
    step = step + 1
    motolines = lines.map(_.toCharArray)
  }
}
