package codingames.veryhard.knight

import math._
import scala.io.Source
import scala.io.StdIn._

object Player3 extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
  val filename = "resources/knight/lesser-jumps.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines
  def readInt = if (data.hasNext) data.next.toInt else {System.exit(0); -1}
  def readLine = if (data.hasNext) data.next else {System.exit(0); ""}
// ----------------------------------------------------------------------------------------------------------------------
  type Correct = (Int, Int) => Boolean
  type GetYType = (Boolean, Boolean)
  type GetValue = () => Int
  type GetRanges = () => (Range, Range)
  val Array(w, h) = for (i <- readLine split " ") yield i.toInt
  val n = readInt
  val Array(x_, y_) = for (i <- readLine split " ") yield i.toInt

  var x = x_
  var y = y_
  var x0 = x
  var y0 = y
  var xs = 0 until w
  var ys = 0 until h
  var bombDirection: String = _
  val eq:   Correct = (a, b) => a == b
  val less: Correct = (a, b) => a < b
  val more: Correct = (a, b) => a > b
  val all:  Correct = (_, _) => true
  val justY: GetValue = () => y
  val xsUpdated: GetRanges = () => (correctRange(xs, x0, x), ys)
  val ysUpdated: GetRanges = () => (xs, correctRange(ys, y0, y))
  val rangeXHead: GetValue = () => xs.head
  val rangeYHead: GetValue = () => ys.head
  val minimaxY: GetValue = () => minimax(newCoord(ys, y0, h), h)
  val minimaxX: GetValue = () => {
    val xx = newCoord(xs, x0, w)
    minimax(if (xx == x) xx + 1 else xx, w)
  }

  val correctFunction: Map[String, Correct]  = Map("SAME" -> eq, "WARMER" -> more, "COLDER" -> less)
  val getRangesFunction: Map[Boolean, GetRanges]  = Map(true -> ysUpdated, false -> xsUpdated)
  val getXFunction: Map[Boolean, GetValue]  = Map(true -> rangeXHead, false -> minimaxX)
  val getYFunction: Map[GetYType, GetValue]  = Map(
    (true, true) -> rangeYHead,
    (true, false) -> minimaxY,
    (false, true) -> justY,
    (false, false) -> justY)

  def minimax(point: Int, size: Int) = min(max(point, 0), size - 1)

  def correctRange(range: Range, prev: Int, current: Int) = {
    val seq = range.filter(i => correctFunction.getOrElse(bombDirection, all)(abs(prev - i), abs(current - i)))
    seq.head to seq.last
  }

  def newCoord(range: Range, prev: Int, size: Int) = {    // FIXME
    if (prev == 0 && range.length < size) {
      (3 * range.head + range.last) / 2
    } else if (prev == size - 1 && range.length != size) {
      (range.head + 3 * range.last) / 2 - prev
    } else {
      range.head + range.last - prev
    }
  }

  def additionalEnter(range: Range) = if (range.length == 1 && x != range.head) {
    println(s"${range.head} $y")
    readLine
  } else bombDirection

  for (_ <- LazyList.from(0).takeWhile(_ < n)) {
    bombDirection = readLine
    Console.err.println(s"$bombDirection")
    val (xs_, ys_) = getRangesFunction(xs.length == 1).apply
    xs = xs_
    ys = ys_
    x0 = x
    y0 = y

    bombDirection = additionalEnter(xs)

    x = getXFunction(xs.length == 1).apply
    y = getYFunction((xs.length == 1, ys.length == 1)).apply

    println(s"$x $y")
  }
}
