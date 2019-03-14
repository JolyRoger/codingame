package codingame.challenge.codealamode

import codingames.challenge.codealamode.Player
import codingames.challenge.codealamode.Player.searchSym
import org.scalatest.FlatSpec

class PlayerTests extends FlatSpec {
  val cookBook = Map("BLUEBERRIES-CROISSANT" -> Player.croissantBlueberries,
    "CROISSANT" -> Player.croissant,
    "BLUEBERRIES-ICE_CREAM" -> Player.iceCreamBlueberries,
    "CHOPPED_STRAWBERRIES-ICE_CREAM" -> Player.iceCreamChoppedStrawberries,
    "BLUEBERRIES-CHOPPED_STRAWBERRIES" -> Player.сhoppedStrawberriesBlueberries,
    "BLUEBERRIES-CROISSANT-ICE_CREAM" -> Player.croissantBlueberriesIcecream,
    "BLUEBERRIES-CHOPPED_STRAWBERRIES-ICE_CREAM" -> Player.iceCreamChoppedStrawberriesBlueberries,
    "BLUEBERRIES-CHOPPED_STRAWBERRIES-CROISSANT" -> Player.blueberriesCroissantChoppedStrawberries,
    "BLUEBERRIES-CHOPPED_STRAWBERRIES-CROISSANT-ICE_CREAM" -> Player.iceCreamBlueberriesCroissantChoppedStrawberries,
    "CHOPPED_STRAWBERRIES-CROISSANT" -> Player.croissantChoppedStrawberries,
    "CHOPPED_STRAWBERRIES-CROISSANT-ICE_CREAM" -> Player.iceCreamCroissantChoppedStrawberries,
    "CROISSANT-ICE_CREAM" -> Player.iceCreamCroissant,
  )

  val testMatrix = List(
    "#I###D#####",
    "B0.1......#",
    "#.####.##.#",
    "#.#..#..#.#",
    "#.##.####.#",
    "#.........#",
    "#####W#####"
  )
  val targetMap = Map("DISH" -> searchSym(testMatrix, 'D').getOrElse((-1,-1)),
                      "ICE_CREAM" -> searchSym(testMatrix, 'W').getOrElse((-1,-1)),
                      "BLUEBERRIES" -> searchSym(testMatrix, 'B').getOrElse((-1,-1)),
                      "WINDOW" -> searchSym(testMatrix, 'I').getOrElse((-1,-1)))

  "A Player" should "convert number to matrix and back" in {
    val point = (10, 2)
    val square = 32
    assert(Player.toMatrix(square) == point)
    assert(Player.toNumber(point) == square)
    assert(Player.toNumber(Player.toMatrix(square)) == square)
  }

  "A Player" should "find adjacent squares" in {
    val testPoint = (5, 0)
    val testPoint2 = (9, 3)
    val testPoint3 = (10, 6)
    val res = Player.adjTo(testMatrix, testPoint)
    val res2 = Player.adjTo(testMatrix, testPoint2)
    val res3 = Player.adjTo(testMatrix, testPoint3)
    assert(res == List((4, 1), (5, 1), (6, 1)))
    assert(res2 == List((9, 2), (9, 4)))
    assert(res3 == List((9, 5)))
  }

  val targetStaticMap = Map("DISH" -> List((1,1)),
                            "WASH" -> List((2,2)))

  def addToMap(oldMap: Map[String, List[(Int, Int)]], key: String, value: (Int, Int)) =
    oldMap + (key -> (oldMap.get(key) match {
      case Some(lst) => value :: lst
      case None => List(value)
    }))

  "A Player" should "find closest empty table" in {
    val point = (10,7)
    val cet = Player.searchClosestEmptyTable(testMatrix, point)
    Console.err.println(s"Closest empty table to $point is $cet")
  }

  "A Player" should "print target map" in {
    Console.err.println(s"targetStaticMap before: $targetStaticMap")
    val newMap = addToMap(targetStaticMap, "BAR", (4,2))
    Console.err.println(s"new map: $newMap")
    val tdm = addToMap(targetStaticMap, "WASH", (4,2))
    Console.err.println(s"tdm: $tdm")
  }

  "A Player" should "find another target" in {
    val customeritems = List(Array("DISH-ICE_CREAM-BLUEBERRIES", "42"), Array("DISH-ICE_CREAM-BLUEBERRIES", "43"), Array("DISH-ICE_CREAM-BLUEBERRIES", "44"))
    val playeritem = "DISH-ICE_CREAM"
    val nextTarget = Player.nextTarget(playeritem, customeritems, cookBook)
    Console.err.println(s"nextTarget: $nextTarget")
    assert(nextTarget == List("BLUEBERRIES"))
  }

  "A Player" should "find next target" in {
    val customeritems = List(Array("DISH-ICE_CREAM-BLUEBERRIES", "42"), Array("DISH-ICE_CREAM-BLUEBERRIES", "43"), Array("DISH-ICE_CREAM-BLUEBERRIES", "44"))
    //    val customeritemsA = List("DISH-BLUEBERRIES-ICE_CREAM", "DISH-ICE_CREAM-BLUEBERRIES", "DISH-ICE_CREAM-BLUEBERRIES")
    customeritems.foreach(item => Console.err.println(s"customer items: ${item.mkString("[",", ","]")}"))

    val result = List(List("DISH"), List("BLUEBERRIES"), List("ICE_CREAM"), List("WINDOW"), List("BLUEBERRIES", "ICE_CREAM"))
    val playeritem = List("NONE", "DISH-ICE_CREAM", "DISH-BLUEBERRIES", "DISH-BLUEBERRIES-ICE_CREAM", "DISH")

    val nextTarget = playeritem.map(Player.nextTarget(_, customeritems, cookBook)).zipWithIndex
    Console.err.println
    nextTarget.foreach {
      next => {
        Console.err.println(s"prev state: ${playeritem(next._2)}, target: ${next._1}")
        //        assert(next._1 == result(next._2))
      }
    }
  }
}
