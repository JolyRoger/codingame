package codingames.hard.bridge.v2

object Player extends App {
  type Point = (Int, Int)
  type PathData = (Int, (Int, Int, Int))
  type MotoData = (Int, Int, Int, List[(Int, Int)])
  type Rule = (Int, Int, Int) => MotoData
  type Command = Map[String, Rule]

  val m = readInt // the amount of motorbikes to control
  val v = readInt // the minimum amount of motorbikes that must survive
  val l0 = readLine // L0 to L3 are lanes of the road. A dot character . represents a safe space, a zero 0 represents a hole in the road.
  val l1 = readLine
  val l2 = readLine
  val l3 = readLine

  def commands = Map("SPEED" -> ((x: Int, y: Int, s: Int) => {
    (x + s + 1, y, s + 1, (x to x + s + 1).map((_, y)).toList)
  }), "SLOW" -> ((x: Int, y: Int, s: Int) => {
    (x + s - 1, y, s - 1, (x until x + s).map((_, y)).toList)
  }), "JUMP" -> ((x: Int, y: Int, s: Int) => {
    (x + s, y, s, List((x, y), (x + s, y)))
  }), "WAIT" -> ((x: Int, y: Int, s: Int) => {
    (x + s, y, s, (x to x + s).map((_, y)).toList)
  }), "UP" -> ((x: Int, y: Int, s: Int) => {
    (x + s, y - 1, s, ((x until x + s).map((_, y)) :: (x + 1 to x + s).map((_, y - 1)) :: Nil).flatten)
  }), "DOWN" -> ((x: Int, y: Int, s: Int) => {
    (x + s, y + 1, s, ((x until x + s).map((_, y)) :: (x + 1 to x + s).map((_, y + 1)) :: Nil).flatten)
  }))

  val lines = Array(l0, l1, l2, l3)

  val roadLength = lines(0).length

  var motolines = lines.map(_.toCharArray)
  Console.err.println(s"the amount of motorbikes to control: $m")
  Console.err.println(s"the minimum amount of motorbikes that must survive: $v")

  def printLines(lines: Array[Array[Char]]) = lines.foreach(line => Console.err.println(s"lanes of the road: ${line.mkString("", "", "")}"))

  var step = 0

  val order = Map(0 -> "SPEED", 1 -> "JUMP", 2 -> "WAIT", 3 -> "UP", 4 -> "DOWN", 5 -> "SLOW")


  def put(stack: List[PathData], e: PathData) = {
    e :: stack
  }

  def newXYS(stack: List[PathData]) = {
    val (c, (x, y, s)) = stack.head
    commands(order(c))(x, y, s)
  }

  def isActive(stack: List[PathData]) = stack.size < 50 && stack.head._2._1 < roadLength

  def can(data: MotoData) = data._4.forall(xy => {
      val (sym, line) = xy
      line >= 0 && line < 4 &&
        sym >= 0 && (if (sym < roadLength) lines(line)(sym) == '.' else true)
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
    var stack = (0, (initX, initY, initS)) :: Nil
    do {
      val (newX, newY, newS, points) = newXYS(stack)
      stack = if (can((newX, newY, newS, points))) put(stack, (0, (newX, newY, newS))) else update(stack)
    } while (isActive(stack))
    stack.tail
  }.reverse

  var instructions: List[PathData] = List.empty

  while (true) {
    val s = readInt // the motorbikes' speed
    for (i <- 0 until m) {
      val Array(x, y, a) = for (j <- readLine split " ") yield j.toInt
      if (step == 0) instructions = calc(x, y, s)
      if (x < motolines(y).length) motolines(y)(x) = '*'
    }
    printLines(motolines)

    val action = order(instructions(step)._1)
    step = step + 1
    println(action)
    motolines = lines.map(_.toCharArray)
  }
}
