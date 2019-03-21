// package codingames.hard.bridge

import math._
import scala.util._

// x,y — location, s — speed which the node has been attained

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

  val commands = Map("SPEED" -> ((x: Int, y: Int, s: Int) => {
    (x + s, y, s + 1, (x to s).map((_, y)).toList)
  }), "SLOW" -> ((x: Int, y: Int, s: Int) => {
    (x + s, y, s - 1, (x to s).map((_, y)).toList)
  }), "JUMP" -> ((x: Int, y: Int, s: Int) => {
    (x + s, y, s - 1, List((x, y), (x + s, y)))
  }), "WAIT" -> ((x: Int, y: Int, s: Int) => {
    (x + s, y, s, (x to s).map((_, y)).toList)
  }), "UP" -> ((x: Int, y: Int, s: Int) => {
    (x + s, y - 1, s, ((x until s).map((_, y)) :: (x + 1 to s).map((_, y - 1)) :: Nil).flatten)
  }), "DOWN" -> ((x: Int, y: Int, s: Int) => {
    (x + s, y + 1, s, ((x until s).map((_, y)) :: (x + 1 to s).map((_, y + 1)) :: Nil).flatten)
  }))

  val lines = Array(l0, l1, l2, l3)
  var motolines = lines.map(_.toCharArray)
  Console.err.println(s"the amount of motorbikes to control: $m")
  Console.err.println(s"the minimum amount of motorbikes that must survive: $v")

  def printLines(lines: Array[Array[Char]]) = lines.foreach(line => Console.err.println(s"lanes of the road: ${line.mkString("", "", "")}"))

  var c = 0


  val order = Map("NONE" -> "SPEED", "SPEED" -> "JUMP", "JUMP" -> "WAIT", "WAIT" -> "UP", "UP" -> "DOWN", "DOWN" -> "SLOW", "SLOW" -> "EMPTY")
  val order2 = Map(0 -> "SPEED", 1 -> "JUMP", 2 -> "WAIT", 3 -> "UP", 4 -> "DOWN", 5 -> "SLOW", 6-> "EMPTY")


  def put(stack: List[PathData], e: PathData) = {
    e :: stack
  }

  def newXYS(stack: List[PathData], e: Int) = {
    val (c, (x,y,s)) = stack.head
    commands(order2(e))(x, y, s)
  }

  def isActive(steps: Int) = steps < 5

  def can(data: MotoData): Boolean = {
    true
  }

  def calc = {
    var command = 0
    var newStack = List.empty[PathData]

    while (isActive(newStack.size)) {
      val (newX, newY, newS, points) = newXYS(newStack, command)
      if (can((newX, newY, newS, points))) {
        newStack = put(newStack, (command, (newX, newY, newS)))
      } else {
        if (command == 5) {
          command = newStack.head._1 + 1
          newStack = newStack.tail
        } else {
          command = command + 1
        }
      }
    }
    newStack
  }




  // game loop
  while (true) {
    c = c + 1
    val s = readInt // the motorbikes' speed
    Console.err.println(s"speed=$s")
    for (i <- 0 until m) {
      // x: x coordinate of the motorbike
      // y: y coordinate of the motorbike
      // a: indicates whether the motorbike is activated "1" or detroyed "0"
      val Array(x, y, a) = for (j <- readLine split " ") yield j.toInt
      Console.err.println(s"x[$i]=$x\ty[$i]=$y\ta[$i]=$a")
      if (x < motolines(y).length) motolines(y)(x) = '*'
    }
    printLines(motolines)


    // Write an action using println
    // To debug: Console.err.println("Debug messages...")


    // A single line containing one of 6 keywords: SPEED, SLOW, JUMP, WAIT, UP, DOWN.
    val action = if (c == 5) "JUMP" else if (c > 5) "WAIT" else "SPEED"
    println(action)
//    println("UP")
    motolines = lines.map(_.toCharArray)
  }
}
