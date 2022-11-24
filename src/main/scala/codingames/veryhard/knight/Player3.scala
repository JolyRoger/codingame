package codingames.veryhard.knight

import math._
import scala.io.Source
import scala.io.StdIn._
import scala.reflect.ClassTag
import scala.util.Random

object Player3 extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
  // val filename = "resources/knight/lot-of-windows.txt"
  //     val filename = "resources/knight/lot-of-jumps.txt"
  //  val filename = "resources/knight/tower.txt"
  // val filename = "resources/knight/lesser-jumps.txt"
  //    val filename = "resources/knight/more-window.txt"
  val filename = "resources/knight/lesser-jumps.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines
  def readInt = if (data.hasNext) data.next.toInt else {System.exit(0); -1}
  def readLine = if (data.hasNext) data.next else {System.exit(0); ""}
// ----------------------------------------------------------------------------------------------------------------------
  val Array(w, h) = for (i <- readLine split " ") yield i.toInt
  Console.err.println(s"$w $h")
  val n = readInt
  Console.err.println(s"$n")
  val Array(x_, y_) = for (i <- readLine split " ") yield i.toInt
  Console.err.println(s"$x_ $y_")

  var x = x_
  var y = y_
  var x0 = x
  var y0 = y
  var xs = 0 until w
  var ys = 0 until h

  def narrow(x0: Int, y0: Int, x: Int, y: Int, xs: Range, ys: Range, info: String) = {
    Console.err.println(s"narrow : x0=$x0, y0=$y0, x=$x, y=$y, info=$info")
    val newXs = if (xs.length != 1) {
      if (info == "SAME") {
        val seq = xs.filter(i => abs(x0 - i) == abs(x - i))  // [i for i in xs if abs(x0-i) == abs(x-i)]
        seq.head to seq.last
      } else if (info == "WARMER") {
        val seq = xs.filter(i => abs(x0 - i) > abs(x - i))  // [i for i in xs if abs(x0-i) == abs(x-i)]
        seq.head to seq.last
      } else if (info == "COLDER") {
        val seq = xs.filter(i => abs(x0 - i) < abs(x - i))  // [i for i in xs if abs(x0-i) == abs(x-i)]
        seq.head to seq.last
      } else xs
    } else xs

    val newYs = if (ys.length != 1 && xs.length == 1) {
      if (info == "SAME") {
        val seq = ys.filter(i => abs(y0 - i) == abs(y - i))  // [i for i in xs if abs(x0-i) == abs(x-i)]
        seq.head to seq.last
      } else if (info == "WARMER") {
        val seq = ys.filter(i => abs(y0 - i) > abs(y - i))  // [i for i in xs if abs(x0-i) == abs(x-i)]
        seq.head to seq.last
      } else if (info == "COLDER") {
        val seq = ys.filter(i => abs(y0 - i) < abs(y - i))  // [i for i in xs if abs(x0-i) == abs(x-i)]
        seq.head to seq.last
      } else ys
    } else ys

    Console.err.println(s"$newXs")
    Console.err.println(s"$newYs")

    (newXs, newYs)
  }


  for (_ <- LazyList.from(0).takeWhile(_ < n)) {
    var info = readLine
    Console.err.println(s"$info")
    val (xs_, ys_) = narrow(x0, y0, x, y, xs, ys, info)
    xs = xs_
    ys = ys_
    x0 = x
    y0 = y
    Console.err.println(s"x=$x y=$y")

    if (xs.length > 1) {
      val xx = if (x0 == 0 && xs.length < w) {
        (3 * xs.head + xs.last) / 2 - x0
      } else if (x0 == w - 1 && xs.length != w) {
        (xs.head + 3 * xs.last) / 2 - x0
      } else {
        xs.head + xs.last - x0
      }
      x = if (xx == x) xx + 1 else xx
      x = min(max(x, 0), w-1)
    } else {
      if (x != xs.head) {
        x0 = xs.head
        x = x0
        println(s"$x $y")
        info = readLine
        Console.err.println(s"another info $info")
      }
      //     finishing
      if (ys.length == 1) {
        y = ys.head
      } else {
        if (y0 == 0 && ys.length != h) {
          y = (3 * ys.head + ys.last) / 2 - y0
        } else if (y0 == h - 1 && ys.length != h) {
          y = (ys.head + 3 * ys.last) / 2 - y0
        } else {
          y = ys.head + ys.last - y0
        }
        y = min(max(y, 0), h-1)
      }
    }
    println(s"$x $y")
  }
}
