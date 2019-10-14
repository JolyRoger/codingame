package codingame.easy.asciiart

import math._
import scala.util._
import scala.io.StdIn._


/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Solution extends App {
  val L = readLine.toInt
  Console.err.println(s"$L")
  val H = readLine.toInt
  Console.err.println(s"$H")
  val T = readLine
  Console.err.println(s"$T")
  // var data: Map[Char, Array[Array[Char]] = Map.empty

  val data = for (i <- 0 until H) yield readLine split ""

  def process(in: Array[String], chunk: Int): Array[String] = {
    val builder = new StringBuilder
    val out = new Array[String](in.length / chunk)

    for (i <- in.indices)
      if (i % chunk == chunk - 1) {
        builder.append(in(i))
        out(i / chunk) = builder.toString
        builder.clear
      } else builder.append(in(i))
    out
  }


  val res = data.map {
    line =>
  }
  data.foreach(line => {
    Console.err.println(s"${line.mkString("", "", "")}")
  })

  // Write an action using println
  // To debug: Console.err.println("Debug messages...")
  Console.err.println
  Console.err.println
  println("### ")
  println("#   ")
  println("##  ")
  println("#   ")
  println("### ")
}