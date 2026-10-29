package codingames.medium.network

import math._
import scala.io.Source
import scala.util._
import scala.io.StdIn._

object Solution extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
  val filename = "resources/network/4.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines
  def readInt = if (data.hasNext) data.next.toInt else {System.exit(0); -1}
  def readLine = if (data.hasNext) data.next else {System.exit(0); ""}
//----------------------------------------------------------------------------------------------------------------------
  val n = readLine.toInt
  val res = for(_ <- 0 until n) yield {
    val Array(x, y) = (readLine split "\\s").withFilter(_.nonEmpty).map(_.toLong)
    (x, y)
  }

  val (xs, ys) = res.unzip
  val ymax = ys.max
  val ymin = ys.min
  val deltaY = ymax - ymin

  val xmax = xs.max
  val xmin = xs.min
  val deltaX = xmax - xmin


  val optimal = if (deltaY < 5) {
    (ymin to ymax).map(
      cableY => {
        val itogo = res.map(r => Math.abs(r._2 - cableY)).sum
        Console.err.println(s"LINE=$cableY\tMin=$itogo")
        itogo
      }
    ).min
  } else {
    val sortedRes = res.sortBy(_._2)
    val half = res.size / 2
    val step = 1
    var cableY = sortedRes(half)._2
    var itogo = res.map(r => Math.abs(r._2 - cableY)).sum
    val left = cableY - step
    val right = cableY + step
    val itogoLeft = res.map(r => Math.abs(r._2 - left)).sum
    val itogoRight = res.map(r => Math.abs(r._2 - right)).sum


    if (itogo < itogoLeft && itogo < itogoRight) itogo
    else {
      val f = if (itogoLeft < itogo) (a: Long) => a + 1 else (a: Long) => a - 1

      var proceed = true

      while (proceed) {
        cableY = f(cableY)
        val newItogo = res.map(r => Math.abs(r._2 - cableY)).sum
        if (newItogo < itogo) {
          itogo = newItogo
          proceed = true
        } else proceed = false
      }
      itogo
    }
  }

  println(s"${optimal + deltaX}")
}
