package codingames.hard.thelastcrusade

import scala.io.Source
import scala.io.StdIn._

object Player extends App {

//------------------------------------------FILE ENTRY------------------------------------------------------------------
  val filename = "resources/thelastcrusade/BrokenWell.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines
  def readInt = if (data.hasNext) data.next.toInt else -1
  def readLine = if (data.hasNext) data.next else "EOF"
//------------------------------------------FILE ENTRY------------------------------------------------------------------
  var i = 0

  val enter = Array(List("2", "1", "RIGHT"), List("WAIT"), List("WAIT"))

  // w: number of columns.
  // h: number of rows.
  val Array(w, h) = (readLine split " ").map(_.toInt)
  Console.err.println(s"$w $h")
  for (i <- 0 until h) {
    val line = readLine // each line represents a line in the grid and contains W integers T. The absolute value of T specifies the type of the room. If T is negative, the room cannot be rotated.
    Console.err.println(s"$line")
  }
  val ex = readLine.toInt // the coordinate along the X axis of the exit.
  Console.err.println(s"$ex")

  while (true) {
    val Array(_xi, _yi, posi) = readLine split " "
    val xi = _xi.toInt
    val yi = _yi.toInt
    Console.err.println(s"$xi $yi $posi") // Indiana position and where he came from
    val r = readLine.toInt // the number of rocks currently in the grid.
    Console.err.println(s"$r")
    for (i <- 0 until r) {
      val Array(_xr, _yr, posr) = readLine split " "
      val xr = _xr.toInt
      val yr = _yr.toInt
      Console.err.println(s"$xr $yr $posr")
    }

    Console.err.println(s"-----------------")

    println(s"${enter(i).mkString(" ").trim}")
    i += 1
  }
}
