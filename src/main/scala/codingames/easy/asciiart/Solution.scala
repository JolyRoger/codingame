//package codingame.easy.asciiart

import scala.collection.mutable
import scala.io.StdIn._

object Solution extends App {
  val symLength = readLine.toInt
  val symWidth = readLine.toInt
  val text = readLine.toUpperCase.replaceAll("\\W", "?")

  val asciiData = "ABCDEFGHIJKLMNOPQRSTUVWXYZ?".split("").map(_.toCharArray()(0)).zipWithIndex.toMap

  def process(in: Array[String], chunk: Int) = {
    val builder = new StringBuilder
    val out = new Array[String](in.length / chunk)

    for (i <- in.indices)
      if (i % chunk == chunk - 1) {
        val c = i / chunk
        builder.append(in(i))
        out(c) = builder.toString
        builder.clear
      } else builder.append(in(i))
    out
  }

  def getLine(text: String, data: Array[String]) = {
    text.split("").map(_.toCharArray()(0)).map(ch => data(asciiData(ch))).reduce(_ + _)
  }

  (for (i <- 0 until symWidth) yield readLine split "").foreach { line =>
    println(getLine(text, process(line, symLength)))
  }
}