package codingames.challenge.harvest

import math._
import scala.io.Source
import scala.util._
import scala.io.StdIn._

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/

case class Hexagon(resources: Int, myAnts: Int, oppAnts: Int) {

}

object Player extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
   val filename = "resources/harvest/1.txt"
   val bufferedSource = Source.fromFile(filename)
   val data = bufferedSource.getLines
   def readInt = if (data.hasNext) data.next.toInt else {System.exit(0); -1}
   def readLine = if (data.hasNext) data.next else {System.exit(0); ""}
//----------------------------------------------------------------------------------------------------------------------
  val n = 100
  val numberOfCells = readLine.toInt // amount of hexagonal cells in this map
  Console.err.println(s"$numberOfCells")
  for(i <- 0 until numberOfCells) {
    // _type: 0 for empty, 1 for eggs, 2 for crystal
    // initialResources: the initial amount of eggs/crystals on this cell
    // neigh0: the index of the neighbouring cell for each direction
    val Array(_type, initialResources, neigh0, neigh1, neigh2, neigh3, neigh4, neigh5) = (readLine split " ").filter(_ != "").map (_.toInt)
    Console.err.println(s"${_type}, $initialResources, $neigh0, $neigh1, $neigh2, $neigh3, $neigh4, $neigh5")
  }
  val numberOfBases = readLine.toInt
  Console.err.println(s"$numberOfBases")
  var inps = readLine
  Console.err.println(s"$inps")
  var inputs = inps split "\\s"
  for(i <- 0 until numberOfBases) {
    val myBaseIndex = inputs(i).toInt
  }

  inps = readLine
  Console.err.println(s"$inps")
  inputs = inps split "\\s"
  for(i <- 0 until numberOfBases) {
    val oppBaseIndex = inputs(i).toInt
  }

  val hexagons = Array.ofDim[Hexagon](numberOfCells)

  for (step <- LazyList.from(0).takeWhile(_ < n)) {
    for(i <- 0 until numberOfCells) {
      // resources: the current amount of eggs/crystals on this cell
      // myAnts: the amount of your ants on this cell
      // oppAnts: the amount of opponent ants on this cell
      val Array(resources, myAnts, oppAnts) = (readLine split "\\s").withFilter(_.nonEmpty).map(_.toInt)
      hexagons(i) = Hexagon(resources, myAnts, oppAnts)
      Console.err.println(s"$resources, $myAnts, $oppAnts")
    }

    // Write an action using println
    // To debug: Console.err.println("Debug messages...")


    // WAIT | LINE <sourceIdx> <targetIdx> <strength> | BEACON <cellIdx> <strength> | MESSAGE <text>
    println("WAIT")
  }
}