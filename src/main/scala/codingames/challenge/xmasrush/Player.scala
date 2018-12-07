//package codingames.challenge.xmasrush

import math._
import scala.util._

/**
 * Help the Christmas elves fetch presents in a magical labyrinth!
 **/
object Player extends App {

  def closest(p: (Int, Int), items: List[(Int, Int)]) = items.minBy(item1 => euclidean(p, item1))
  def euclidean(a: (Int, Int), b: (Int, Int)) = sqrt(pow(b._1 - a._1, 2) + pow(b._2 - a._2, 2))

    // game loop
    while(true) {
        val turntype = readInt
        Console.err.println("turntype: " + turntype)
        Console.err.println("++++++++++++++++++++++++++++")
        for(i <- 0 until 7) {
            var inputs = readLine split " "
//            inputs.foreach(Console.err.println)
            for(j <- 0 until 7) {
                
            }
//            Console.err.println("------------------------")
        }
        Console.err.println("++++++++++++++++++++++++++++")
        for(i <- 0 until 2) {
            // numplayercards: the total number of quests for a player (hidden and revealed)
            val Array(_numplayercards, _playerx, _playery, playertile) = readLine split " "
            val numplayercards = _numplayercards.toInt
            val playerx = _playerx.toInt
            val playery = _playery.toInt
            Console.err.println("numplayercards: " + numplayercards)
            Console.err.println("playerx: " + playerx)
            Console.err.println("playery: " + playery)
            Console.err.println("playertile: " + playertile)
            Console.err.println("------------------------")
        }
        Console.err.println("++++++++++++++++++++++++++++")
        val numitems = readInt // the total number of items available on board and on player tiles
        for(i <- 0 until numitems) {
            val Array(itemname, _itemx, _itemy, _itemplayerid) = readLine split " "
            val itemx = _itemx.toInt
            val itemy = _itemy.toInt
            val itemplayerid = _itemplayerid.toInt
            Console.err.println("itemname: " + itemname)
            Console.err.println("itemx: " + itemx)
            Console.err.println("itemy: " + itemy)
            Console.err.println("itemplayerid: " + itemplayerid)
            Console.err.println("------------------------")
        }
        
        val numquests = readInt // the total number of revealed quests for both players
        for(i <- 0 until numquests) {
            val Array(questitemname, _questplayerid) = readLine split " "
            val questplayerid = _questplayerid.toInt
            Console.err.println("questplayerid: " + questplayerid)
            Console.err.println("------------------------")
        }
        Console.err.println("+++++++++++++++++++++++++++++++++++++++")
        // Write an action using println
        // To debug: Console.err.println("Debug messages...")


        println("PUSH 6 RIGHT") // PUSH <id> <direction> | MOVE <direction> | PASS
    }
}