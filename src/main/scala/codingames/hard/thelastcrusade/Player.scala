//package codingames.hard.thelastcrusade

import math._
import scala.util._

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Player extends App {
    // w: number of columns.
    // h: number of rows.
    val Array(w, h) = for(i <- readLine split " ") yield i.toInt
	Console.err.println(s"$w $h")

    for(i <- 0 until h) {
        val line = readLine // each line represents a line in the grid and contains W integers T. The absolute value of T specifies the type of the room. If T is negative, the room cannot be rotated.
		Console.err.println(s"$line")
    }
    val ex = readInt // the coordinate along the X axis of the exit.
	Console.err.println(s"$ex")

    // game loop
    while(true) {
        val Array(_xi, _yi, posi) = readLine split " "
        val xi = _xi.toInt
        val yi = _yi.toInt
        val r = readInt // the number of rocks currently in the grid.
        for(i <- 0 until r) {
            val Array(_xr, _yr, posr) = readLine split " "
            val xr = _xr.toInt
            val yr = _yr.toInt
        }
        
        // Write an action using println
        // To debug: Console.err.println("Debug messages...")
        

        // One line containing on of three commands: 'X Y LEFT', 'X Y RIGHT' or 'WAIT'
        println("WAIT")
    }
}