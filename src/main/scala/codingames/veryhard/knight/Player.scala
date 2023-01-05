package codingames.veryhard.knight

import math.{abs, min, max}
import scala.io.StdIn.{readLine, readInt}

object Player extends App {
  type Correct = (Int, Int) => Boolean
  type GetType = (Boolean, Boolean)
  type GetValue = () => Int
  type GetRanges = () => (Range, Range)
  type CalcValue = (Range, Int) => Int

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
  val xsUpdated: GetRanges = () => (correctRange(xs, x0, x), ys)
  val ysUpdated: GetRanges = () => (xs, correctRange(ys, y0, y))
  val firstPrev: CalcValue = (range, prev) => calc(3, 2, range.head, range.last, prev)
  val lastPrev: CalcValue = (range, prev) => calc(3, 2, range.last, range.head, prev)
  val midPrev: CalcValue = (range, prev) => calc(1, 1, range.head, range.last, prev)
  val justY: GetValue = () => y
  val rangeXHead: GetValue = () => xs.head
  val rangeYHead: GetValue = () => ys.head
  val minimaxY: GetValue = () => minimax(newCoord(ys, y0, h), h)
  val minimaxX: GetValue = () => {
    val xx = newCoord(xs, x0, w)
    minimax(if (xx == x) xx + 1 else xx, w)
  }

  val correctFunction: Map[String, Correct] = Map("SAME" -> eq, "WARMER" -> more, "COLDER" -> less)
  val getRangesFunction: Map[Boolean, GetRanges] = Map(true -> ysUpdated, false -> xsUpdated)
  val getXFunction: Map[Boolean, GetValue] = Map(true -> rangeXHead, false -> minimaxX)
  val getYFunction: Map[GetType, GetValue] = Map(
    (true, true) -> rangeYHead,
    (true, false) -> minimaxY,
    (false, true) -> justY,
    (false, false) -> justY)

  val calcFunction: Map[GetType, CalcValue]  = Map(
    (true, true) -> firstPrev,
    (true, false) -> firstPrev,
    (false, true) -> lastPrev,
    (false, false) -> midPrev)

  def minimax(point: Int, size: Int) = min(max(point, 0), size - 1)
  def calc(factor: Int, division: Int, first: Int, second: Int, prev: Int) = (factor * first + second) / division - prev
  def newCoord(range: Range, prev: Int, size: Int) = calcFunction((prev == 0, prev == size - 1))(range, prev)

  def correctRange(range: Range, prev: Int, current: Int) = {
    val seq = range.filter(i => correctFunction.getOrElse(bombDirection, all)(abs(prev - i), abs(current - i)))
    seq.head to seq.last
  }

  def additionalEnter(range: Range) = if (range.length == 1 && x != range.head) {
    println(s"${range.head} $y")
    readLine
  } else bombDirection

  for (_ <- LazyList.from(0).takeWhile(_ < n)) {
    bombDirection = readLine
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
