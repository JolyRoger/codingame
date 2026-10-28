package codingames.medium.network

import math._
import scala.io.Source
import scala.util._
import scala.io.StdIn._

object Solution extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
  val filename = "resources/network/1.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines
  def readInt = if (data.hasNext) data.next.toInt else {System.exit(0); -1}
  def readLine = if (data.hasNext) data.next else {System.exit(0); ""}
//----------------------------------------------------------------------------------------------------------------------
  val n = readLine.toInt
  Console.err.println(s"$n")
  val res = for(_ <- 0 until n) yield {
    val Array(x, y) = (readLine split "\\s").withFilter(_.nonEmpty).map(_.toInt)
    Console.err.println(s"$x $y")
    (x, y)
  }

  val (xs, ys) = res.unzip
  val ymax = ys.max
  val ymin = ys.min

  val xmax = xs.max
  val xmin = xs.min
  val deltaX = xmax - xmin

  val optimal = (ymin to ymax).map(
    cableY => {
      val itogo = res.map(r => Math.abs(r._2 - cableY)).sum
//      Console.err.println(s"LINE=$cableY\tMin=$itogo")
      itogo
    }
  ).min

  println(s"${optimal + deltaX}")
}
