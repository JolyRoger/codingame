//package codingames.challenge.pacman

import math._
import scala.util._
import scala.io.StdIn._

/**
 * Grab the pellets as fast as you can!
 **/
object Player extends App {
    // width: size of the grid
    // height: top left corner is (x=0, y=0)
    val Array(width, height) = (readLine split " ").map (_.toInt)
    Console.err.println(s"${width}x$height")
    for(i <- 0 until height) {
        val row = readLine // one line of the grid: space " " is floor, pound "#" is wall
        Console.err.println(s"\t$row")
    }

    // game loop
    while(true) {
        val Array(myScore, opponentScore) = (readLine split " ").map (_.toInt)
        Console.err.println(s"$myScore : $opponentScore")
        val visiblePacCount = readLine.toInt // all your pacs and enemy pacs in sight
        Console.err.println(s"visiblePacCount=$visiblePacCount")
        for(i <- 0 until visiblePacCount) {
            // pacId: pac number (unique within a team)
            // mine: true if this pac is yours
            // x: position in the grid
            // y: position in the grid
            // typeId: unused in wood leagues
            // speedTurnsLeft: unused in wood leagues
            // abilityCooldown: unused in wood leagues
            val Array(_pacId, _mine, _x, _y, typeId, _speedTurnsLeft, _abilityCooldown) = readLine split " "
            val pacId = _pacId.toInt
            val mine = _mine.toInt != 0
            val x = _x.toInt
            val y = _y.toInt
            val speedTurnsLeft = _speedTurnsLeft.toInt
            val abilityCooldown = _abilityCooldown.toInt
            Console.err.println(s"\t$pacId, $mine, $x, $y, $typeId, $speedTurnsLeft, $abilityCooldown")
        }
        val visiblePelletCount = readLine.toInt // all pellets in sight
        Console.err.println(s"visiblePelletCount=$visiblePelletCount")
        for(i <- 0 until visiblePelletCount) {
            // value: amount of points this pellet is worth
            val Array(x, y, value) = (readLine split " ").map (_.toInt)
            Console.err.println(s"\t$x $y $value")
        }
        
        // Write an action using println
        // To debug: Console.err.println("Debug messages...")
        
        println("MOVE 0 15 10") // MOVE <pacId> <x> <y>
    }
}