package codingames.challenge.platinum

import math._
import scala.io.Source
import scala.util._
import scala.io.StdIn._

class Zone(val zid: Int, var mine: Boolean, var mypods: Int, var oppods: Int, var visible: Boolean, var platinum: Int) {
  var rating = 0
}

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Player extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
      val filename = "resources/platinum/pl.txt"
      val bufferedSource = Source.fromFile(filename)
      val data = bufferedSource.getLines
      def readInt = if (data.hasNext) data.next.toInt else -1
      def readLine = if (data.hasNext) data.next else "EOF"
//----------------------------------------------------------------------------------------------------------------------

  def fillEdge(str: Array[String]) {
    val from = str(0).toInt
    val to = str(1).toInt
    //      Console.err.println(s"$from $to")
    edgeData(from) += to
    edgeData(to) += from
  }
  def smartDivide(dividend: Int, divider: Int) = {
    if (divider == 0) List.empty[Int] else {

    val res = dividend / divider
    var rest = dividend % divider
    (for (i <- (0 until divider)) yield
      res + { if (rest > 0) {
        rest -= 1
        1
      } else 0 }).toList
    }
  }

  // playerCount: the amount of players (always 2)
  // myId: my player ID (0 or 1)
  // zoneCount: the amount of zones on the map
  // linkCount: the amount of links between all zonest
  val Array(playerCount, myId, zoneCount, linkCount) = (readLine split " ").map(_.toInt)
  //      Console.err.println(s"$playerCount $myId $zoneCount $linkCount")
//  //      Console.err.println(s"zone data -----------")
  for (i <- 0 until zoneCount) {
    // zoneId: this zone's ID (between 0 and zoneCount-1)
    // platinumSource: Because of the fog, will always be 0
    val Array(zoneId, platinumSource) = (readLine split " ").map(_.toInt)
     //      Console.err.println(s"$zoneId $platinumSource")
  }
//  //      Console.err.println(s"link data -----------")

  val edgeData = Array.fill[Set[Int]](zoneCount)(Set.empty[Int])
  val zoneData = new Array[Zone](zoneCount)
  var mypods: Set[Zone] = _
  var oppods: Set[Zone] = _



  for (i <- 0 until linkCount) fillEdge(readLine split " ")

//  //      Console.err.println

  // game loop
  while (true) {
    val myPlatinum = readLine.toInt // your available Platinum
    Console.err.println(s"myPlatinum=$myPlatinum")

    mypods = Set.empty[Zone]
    oppods = Set.empty[Zone]

    for (i <- 0 until zoneCount) {
      // zId: this zone's ID
      // ownerId: the player who owns this zone (-1 otherwise)
      // podsP0: player 0's PODs on this zone
      // podsP1: player 1's PODs on this zone
      // visible: 1 if one of your units can see this tile, else 0
      // platinum: the amount of Platinum this zone can provide (0 if hidden by fog)
      val Array(zid, ownerId, podsP0, podsP1, visible, platinum) = (readLine split " ").map(_.toInt)
      if (zoneData(zid) == null) zoneData(zid) = new Zone(zid,
                                                          ownerId == myId,
                                                          if (myId == 0) podsP0 else podsP1,
                                                          if (myId == 0) podsP1 else podsP0,
                                                          visible == 1,
                                                          platinum) else {
                                                        zoneData(zid).mine = ownerId == myId
                                                        zoneData(zid).mypods = if (myId == 0) podsP0 else podsP1
                                                        zoneData(zid).oppods = if (myId == 0) podsP1 else podsP0
                                                        zoneData(zid).visible = visible == 1
                                                        zoneData(zid).platinum = platinum }
      if (zoneData(zid).mypods > 0) {
        zoneData(zid).rating += 1
        mypods = mypods + zoneData(zid)
      }
      if (zoneData(zid).oppods > 0) oppods = oppods + zoneData(zid)

      //      Console.err.println(s"$zid $ownerId $podsP0 $podsP1 $visible $platinum")
    }

    // Write an action using println
    // To debug: //      Console.err.println("Debug messages...")

    var taken = Set.empty[Int]

    val targetStr = mypods.map(zone => {
//    val Array(num, from, to) = mypods.map(zone => {
      val groupedEdges = edgeData(zone.zid).groupBy(edge => zoneData(edge).rating)
      val _edges = groupedEdges(groupedEdges.keys.min)
      val notTakenEdges = _edges -- taken
      val edges = if (notTakenEdges.isEmpty) _edges else notTakenEdges
      val div = smartDivide(zone.mypods, edges.size).zip(edges).filterNot(_._1 == 0)
//      div.map(d1 => (d1._1, zone.zid, d1._2))

      div.map(d1 => {
        taken += d1._2
        s"${d1._1} ${zone.zid} ${d1._2}"
      }).mkString(" ")
    })


    // first line for movement commands, second line no longer used (see the protocol in the statement for details)
    println(s"${targetStr.mkString(" ")}")
    println("WAIT")
  }
}