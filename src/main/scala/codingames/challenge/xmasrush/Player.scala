//package codingames.challenge.xmasrush

import math._
import scala.util._

/**
  * Help the Christmas elves fetch presents in a magical labyrinth!
  **/
object Player extends App {

  def closest(p: (Int, Int), items: List[(Int, Int)]) = {
    Console.err.println("closest::p: " + p)
    items.foreach(item => Console.err.print(item + " * "))
    Console.err.println
    if (items.isEmpty) (-1,-1) else items.minBy(item => euclidean(p, item))
  }
  def euclidean(a: (Int, Int), b: (Int, Int)) = {
    val dist = sqrt(pow(b._1 - a._1, 2) + pow(b._2 - a._2, 2))
    Console.err.println(a + " --> " + b + " = " + dist)
    dist
  }
  def possibleDirections(plSquare: (Int, Int), player: String, neighbours: Array[String]) = {
    val Array(pUp, pRight, pDown, pLeft) = player split ""
//    Console.err.println("Player: " + pUp + " " + pRight + " " + pDown+ " " + pLeft)
//    Console.err.println("Up: " + neighbours(0))
//    Console.err.println("Right: " + neighbours(1))
//    Console.err.println("Down: " + neighbours(2))
//    Console.err.println("Left: " + neighbours(3))
    Console.err.println(s"plSquare: $plSquare")
//    neighbours.foreach(a => Console.err.print(a + ":: "))
//    Console.err.println()
    val upperDown = if (neighbours.head == null) "0" else neighbours.head.split("")(2)
    val rightLeft = if (neighbours(1) == null) "0" else neighbours(1).split("")(3)
    val downUp = if (neighbours(2) == null) "0" else neighbours(2).split("")(0)
    val leftRight = if (neighbours(3) == null) "0" else neighbours(3).split("")(1)

    Map(
      (plSquare._1, plSquare._2 - 1) -> (if (pUp == "1" && pUp == upperDown) "UP" else null),
      (plSquare._1, plSquare._2 + 1) -> (if (pDown == "1" && pDown == downUp) "DOWN" else null),
      (plSquare._1 - 1, plSquare._2) -> (if (pLeft == "1" && pLeft == leftRight) "LEFT" else null),
      (plSquare._1 + 1, plSquare._2) -> (if (pRight == "1" && pRight == rightLeft) "RIGHT" else null)
    ).filter(_._2 != null)
  }

  var i = 0


//    Console.err.println("before")
//  val mmm = Map(null, null)
//    Console.err.println("map " + mmm)
  // game loop
  while (true) {
    val turntype = readInt
//    Console.err.println("turntype: " + turntype)
//    Console.err.println("++++++++++++++++++++++++++++")
//    var tiles: Array[Array[Int]] =
    var inputs = for (i <- 0 until 7) yield readLine split " "

//      for (i <- 0 until 7) {
//      var inputs = readLine split " "
//      inputs(0)(0) /*.flatten.foreach(Console.err.println)*/
//      for (j <- 0 until 7) {
//
//      }
//                  Console.err.println("------------------------")
//    }
//    Console.err.println("++++++++++++++++++++++++++++")

    var players: List[(Int, Int, String)] = Nil

    for (i <- 0 until 2) {
      // numplayercards: the total number of quests for a player (hidden and revealed)
      val Array(_numplayercards, _playerx, _playery, playertile) = readLine split " "
      val numplayercards = _numplayercards.toInt
      val playerx = _playerx.toInt
      val playery = _playery.toInt
      players = (playerx, playery, playertile) :: players
//      Console.err.println("numplayercards: " + numplayercards)
//      Console.err.println("playerx: " + playerx)
//      Console.err.println("playery: " + playery)
//      Console.err.println("playertile: " + playertile)
//      Console.err.println("------------------------")
    }
//    Console.err.println("++++++++++++++++++++++++++++")
    val numitems = readInt // the total number of items available on board and on player tiles

    var items: List[(Int, Int)] = Nil

    for (i <- 0 until numitems) {
      val Array(itemname, _itemx, _itemy, _itemplayerid) = readLine split " "
      val itemx = _itemx.toInt
      val itemy = _itemy.toInt
      val itemplayerid = _itemplayerid.toInt
      items = (itemx, itemy) :: items
//      Console.err.println("itemname: " + itemname)
//      Console.err.println("itemx: " + itemx)
//      Console.err.println("itemy: " + itemy)
//      Console.err.println("itemplayerid: " + itemplayerid)
//      Console.err.println("------------------------")
    }

    val myItem = (items(1)._1, items(1)._2)/*closest((players(1)._1, players(1)._2), items)*/

    val numquests = readInt // the total number of revealed quests for both players
    for (i <- 0 until numquests) {
      val Array(questitemname, _questplayerid) = readLine split " "
      val questplayerid = _questplayerid.toInt
//      Console.err.println("questplayerid: " + questplayerid)
//      Console.err.println("------------------------")
    }
//    Console.err.println("+++++++++++++++++++++++++++++++++++++++")
    // Write an action using println
    // To debug: Console.err.println("Debug messages...")

//    inputs.foreach(a => {
//      Console.err.println()
//      a.foreach(b => Console.err.print(" " + b))
//    }
//    )
//      Console.err.println()
//      Console.err.println()
//    players.foreach(Console.err.println)
//      Console.err.println()
//      Console.err.println()
//    Console.err.println(myItem._1, myItem._2)
//      Console.err.println()
//      Console.err.println()

    val pb = possibleDirections((players(1)._1, players(1)._2), inputs(players(1)._2)(players(1)._1),
      Array(
        if (players(1)._2 - 1 >= 0) inputs(players(1)._2 - 1)(players(1)._1) else null,
        if (players(1)._1 + 1 <= 6) inputs(players(1)._2)(players(1)._1 + 1) else null,
        if (players(1)._2 + 1 <= 6) inputs(players(1)._2 + 1)(players(1)._1) else null,
        if (players(1)._1 - 1 >= 0) inputs(players(1)._2)(players(1)._1 - 1) else null
      )
    )
//    Console.err.println()
//    Console.err.println("Closest:" + closest(myItem, pb.keys.toList))


    val movetype = if (i % 2 == 0) "PUSH" else if (pb.isEmpty) "PASS" else "MOVE"

    var direction =
      if (movetype == "PUSH")
        if (players(1)._1 > myItem._1 && players(1)._2 != myItem._2) "LEFT" else
                          if (players(1)._1 < myItem._1 && players(1)._2 != myItem._2) "RIGHT" else
                          if (players(1)._2 < myItem._2 && players(1)._1 != myItem._1) "DOWN" else
                          if (players(1)._2 > myItem._2 && players(1)._1 != myItem._1) "UP" else "DOWN"
      else if (movetype == "MOVE") {
        Console.err.println("!!!MYITEM!!!: " + myItem)
        pb.foreach(p => Console.err.print(p + " "))
        val cls = closest(myItem, pb.keys.toList)
        Console.err.println("cls: " + cls)
        Console.err.println("pb(cls): " + pb(cls))
        pb(cls)
      } else if (movetype == "PASS") ""

//    Console.err.println("direction: " + direction + " turntype=" + turntype)
//    Console.err.println("movetype: " + movetype)
    val rowid = if (movetype == "PUSH")
      (if (players(1)._1 != myItem._1) myItem._1 else myItem._2) + " "
    else ""


//    Console.err.println(s"$movetype $rowid $direction")
      println(s"$movetype $rowid$direction".trim) // PUSH <id> <direction> | MOVE <direction> | PASS

    i += 1
  }
}
