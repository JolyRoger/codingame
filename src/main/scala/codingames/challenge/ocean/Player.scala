package codingames.challenge.ocean

import math._
import scala.util._
import scala.io.StdIn._

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Player extends App {
    val Array(width, height, myId) = (readLine split " ").map (_.toInt)
    for(i <- 0 until height) {
        val line = readLine
    }
    
    // Write an action using println
    // To debug: Console.err.println("Debug messages...")
    
    println("7 7")

    // game loop
    while(true) {
        val Array(x, y, myLife, oppLife, torpedoCooldown, sonarCooldown, silenceCooldown, mineCooldown) = (readLine split " ").map (_.toInt)
        val sonarResult = readLine
        val opponentOrders = readLine
        
        // Write an action using println
        // To debug: Console.err.println("Debug messages...")
        
        println("MOVE N TORPEDO")
    }
}