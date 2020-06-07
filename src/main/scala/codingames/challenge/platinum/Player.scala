import math._
import scala.util._
import scala.io.StdIn._

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Player extends App {
    // playerCount: the amount of players (always 2)
    // myId: my player ID (0 or 1)
    // zoneCount: the amount of zones on the map
    // linkCount: the amount of links between all zones
    val Array(playerCount, myId, zoneCount, linkCount) = (readLine split " ").map (_.toInt)
    Console.err.println(s"$playerCount $myId $zoneCount $linkCount")
    Console.err.println(s"zone data -----------")
    for(i <- 0 until zoneCount) {
        // zoneId: this zone's ID (between 0 and zoneCount-1)
        // platinumSource: Because of the fog, will always be 0
        val Array(zoneId, platinumSource) = (readLine split " ").map (_.toInt)
        // Console.err.println(s"\t$zoneId $platinumSource")
    }
    Console.err.println(s"link data -----------")
    for(i <- 0 until linkCount) {
        val Array(zone1, zone2) = (readLine split " ").map (_.toInt)
        // Console.err.println(s"\t$zone1 $zone2")
    }

    Console.err.println

    // game loop
    while(true) {
        val myPlatinum = readLine.toInt // your available Platinum
        for(i <- 0 until zoneCount) {
            // zId: this zone's ID
            // ownerId: the player who owns this zone (-1 otherwise)
            // podsP0: player 0's PODs on this zone
            // podsP1: player 1's PODs on this zone
            // visible: 1 if one of your units can see this tile, else 0
            // platinum: the amount of Platinum this zone can provide (0 if hidden by fog)
            val Array(zId, ownerId, podsP0, podsP1, visible, platinum) = (readLine split " ").map (_.toInt)
            Console.err.println(s"$zId $ownerId $podsP0 $podsP1 $visible $platinum")
        }
        
        // Write an action using println
        // To debug: Console.err.println("Debug messages...")
        

        // first line for movement commands, second line no longer used (see the protocol in the statement for details)
        println("WAIT")
        println("WAIT")
    }
}