package codingames.veryhard.knight

import scala.io.Source

object Player  extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
  val filename = "resources/knight/lot-of-jumps.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines
  def readInt = if (data.hasNext) data.next.toInt else {System.exit(0); -1}
  def readLine = if (data.hasNext) data.next else {System.exit(0); ""}
//----------------------------------------------------------------------------------------------------------------------

  def toMatrix(number: Int): (Int, Int) = (number % w, number / w)
  implicit def toNumber(point: (Int, Int)): Int = point._2 * w + point._1 % w

  val Array(w, h) = for (i <- readLine split " ") yield i.toInt
  Console.err.println(s"$w $h")
  val n = readInt
  Console.err.println(n)
  val Array(x0, y0) = for (i <- readLine split " ") yield i.toInt
  Console.err.println(s"${x0} ${y0}")

  var x = x0;
  var y = y0
  val size = w * h

  def firstMove(x: Int, y: Int) = {
    val halfSize = size / 2
    val indexCandidate = toMatrix(halfSize)
    if (indexCandidate == (x, y)) (indexCandidate._1 - 1, indexCandidate._2) else indexCandidate
  }

  def nextMove(x: Int, y: Int, bombdir: String) = {
    if (bombdir == "UNKNOWN") firstMove(x, y)
    else
    (0, 0)
  }

  for (i <- LazyList.from(0).takeWhile(_ < n)) {
    val bombdir = readLine // Current distance to the bomb compared to previous distance (COLDER, WARMER, SAME or UNKNOWN)
    Console.err.println(bombdir)

    val (x_, y_) = nextMove(x, y, bombdir)
    x = x_
    y = y_

    println(s"0 0")
  }
}