package codingame.challenge.codealamode

import codingames.challenge.codealamode.Player
import codingames.challenge.codealamode.Player.searchSym
import org.scalatest.FlatSpec

class PlayerTests extends FlatSpec {
  val cookBook = Map("BLUEBERRIES-CROISSANT-DISH" -> Player.croissantBlueberries,
    "CROISSANT-DISH" -> Player.croissant,
    "BLUEBERRIES-DISH-ICE_CREAM" -> Player.iceCreamBlueberries,
    "CHOPPED_STRAWBERRIES-DISH-ICE_CREAM" -> Player.iceCreamChoppedStrawberries,
    "BLUEBERRIES-CHOPPED_STRAWBERRIES-DISH" -> Player.сhoppedStrawberriesBlueberries,
    "BLUEBERRIES-CHOPPED_STRAWBERRIES-CROISSANT-DISH-TART" -> Player.blueberriesChoppedStrawberriesCroissantTart,
    "BLUEBERRIES-CROISSANT-DISH-ICE_CREAM" -> Player.croissantBlueberriesIcecream,
    "BLUEBERRIES-CHOPPED_STRAWBERRIES-DISH-ICE_CREAM" -> Player.iceCreamChoppedStrawberriesBlueberries,
    "BLUEBERRIES-CHOPPED_STRAWBERRIES-CROISSANT-DISH" -> Player.blueberriesCroissantChoppedStrawberries,
    "BLUEBERRIES-CHOPPED_STRAWBERRIES-CROISSANT-DISH-ICE_CREAM" -> Player.iceCreamBlueberriesCroissantChoppedStrawberries,
    "CHOPPED_STRAWBERRIES-CROISSANT-DISH" -> Player.croissantChoppedStrawberries,
    "CHOPPED_STRAWBERRIES-CROISSANT-DISH-ICE_CREAM" -> Player.iceCreamCroissantChoppedStrawberries,
    "CROISSANT-DISH-ICE_CREAM" -> Player.iceCreamCroissant,
    "CROISSANT-DISH-TART" -> Player.croissantTart,
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
                      "ICE_CREAM" -> searchSym(testMatrix, 'I').getOrElse((-1,-1)),
                      "BLUEBERRIES" -> searchSym(testMatrix, 'B').getOrElse((-1,-1)),
                      "WINDOW" -> searchSym(testMatrix, 'W').getOrElse((-1,-1)))

  val targetStaticMap = Map("DISH" -> List((8, 4)),
    "DISH-TART-CROISSANT" -> List((2, 4)),
    "DISH-TART-CROISSANT-BLUEBERRIES-CHOPPED_STRAWBERRIES" -> List((5, 0)),
    "OVEN" -> List((1, 1)),
    "DOUGH" -> List((2, 0)),
    "WASH" -> List((8, 6)))

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

  def addToMap(oldMap: Map[String, List[(Int, Int)]], key: String, value: (Int, Int)) =
    oldMap + (key -> (oldMap.get(key) match {
      case Some(lst) => value :: lst
      case None => List(value)
    }))

  "A Player" should "split" in {
    val newState = "AA"
    val bb = targetMap.keys.find(key => (if (key.startsWith("TABLE-") || key.startsWith("MOVE-")) key.split("-|#")(1) else key) == newState).getOrElse("TABLE")
    Console.err.println(s"$bb")
  }

  "A Player" should "find closest empty table 2" in {
    val point = (9,5)
    val res = Player.reach(testMatrix, "TABLE", point, targetStaticMap)
    Console.err.println(s"res: $res")
  }
  "A Player" should "find closest empty table" in {
    val point = (10,7)
    val cet = Player.searchClosestEmptyTable(testMatrix, targetStaticMap, point, 1)
    Console.err.println(s"Closest empty table to $point is $cet")
  }

  "A Player" should "print target map" in {
    Console.err.println(s"targetStaticMap before: $targetStaticMap")
    val newMap = addToMap(targetStaticMap, "BAR", (4,2))
    Console.err.println(s"new map: $newMap")
    val tdm = addToMap(targetStaticMap, "WASH", (4,2))
    Console.err.println(s"tdm: $tdm")
  }

  "A Player" should "get target to ready dish" in {
    val player1 = (0,0)
    val player2 = (4,1)

    val target1 = Player.getTargetToReadyDish(player1, "NONE@SOME", "DISH-TART-CROISSANT-BLUEBERRIES-CHOPPED_STRAWBERRIES", targetStaticMap)
    Console.err.println(s"target: $target1")
    assert(target1 == "MOVE@DISH-TART-CROISSANT-BLUEBERRIES-CHOPPED_STRAWBERRIES")
    val target2 = Player.getTargetToReadyDish(player1, "CROISSANT@SOME", "DISH-TART-CROISSANT-BLUEBERRIES-CHOPPED_STRAWBERRIES", targetStaticMap)
    Console.err.println(s"target: $target2")
    val target3 = Player.getTargetToReadyDish(player2, "NONE@SOME", "DISH-TART-CROISSANT-BLUEBERRIES-CHOPPED_STRAWBERRIES", targetStaticMap)
    Console.err.println(s"target: $target3")
    val target4 = Player.getTargetToReadyDish(player2, "CROISSANT@SOME", "DISH-TART-CROISSANT-BLUEBERRIES-CHOPPED_STRAWBERRIES", targetStaticMap)
    Console.err.println(s"target: $target4")
  }

  "A Player" should "get target from game field" in {
    val myState = "DISH-TART"
//    val myOrder = Player.cookBookKey("DISH-TART-CROISSANT-BLUEBERRIES-CHOPPED_STRAWBERRIES")
    val myOrder = Player.cookBookKey("DISH-TART-CROISSANT")
    val customeritems = List(Array("DISH-ICE_CREAM-BLUEBERRIES", "42"), Array("DISH-ICE_CREAM-BLUEBERRIES", "43"),
      Array("DISH-TART-CROISSANT-BLUEBERRIES-CHOPPED_STRAWBERRIES", "815"), Array("DISH-ICE_CREAM-BLUEBERRIES", "44"))
    val cookBookData = customeritems.map(orderData =>  (orderData(0), Player.cookBookKey(orderData(0))))/*.filter {
      pair => cookBook.getOrElse(pair._2, Map.empty[String, String]).contains(myState) /*&& !pair._2.contains("TART")*/
    }*/.sortBy(_._2)
    val res = Player.getTargetFromGameField(myState, customeritems, cookBook, targetStaticMap, cookBook(myOrder), cookBookData)
    Console.err.println(s"target: $res")
  }

  "A Player" should "find another target" in {
//    "CHOPPED_STRAWBERRIES#TART#CROISSANT-ON-A-TABLE" -> "DISH-TART-CROISSANT"
    val customeritems = List(Array("DISH-ICE_CREAM-BLUEBERRIES", "42"), Array("DISH-ICE_CREAM-BLUEBERRIES", "43"),
      Array("BLUEBERRIES-CHOPPED_STRAWBERRIES-CROISSANT-DISH-TART", "815"), Array("DISH-ICE_CREAM-BLUEBERRIES", "44"))
    val myState = "DISH-TART"
    val nextTarget = Player.nextTarget(myState, customeritems, cookBook, targetStaticMap, (0,0))
    Console.err.println(s"nextTarget: $nextTarget")
    assert(nextTarget == "TABLE-DOUGH")
  }

  "A Player" should "set new state" in {
    val state = Player.newState("", "BAR", "OVEN", "KILL")
    Console.err.println(s"new state: $state")
    val newState = Player.newState("", "NONE", "OVEN", "DOUGH")
    Console.err.println(s"new state: $newState")
    val newMoreState = Player.newState("", "ITEM", "OVEN", newState)
    Console.err.println(s"new more state: $newMoreState")
  }
  "A Player" should "find next target" in {
    val customeritems = List(Array("DISH-ICE_CREAM-BLUEBERRIES", "42"), Array("DISH-ICE_CREAM-CHOPPERD_STRAWBERRIES", "43"), Array("DISH-ICE_CREAM-BLUEBERRIES", "44"))
    //    val customeritemsA = List("DISH-BLUEBERRIES-ICE_CREAM", "DISH-ICE_CREAM-BLUEBERRIES", "DISH-ICE_CREAM-BLUEBERRIES")
    customeritems.foreach(item => Console.err.println(s"customer items: ${item.mkString("[",", ","]")}"))

    val result = List(List("DISH"), List("BLUEBERRIES"), List("ICE_CREAM"), List("WINDOW"), List("BLUEBERRIES", "ICE_CREAM"))
    val playeritem = List("NONE", "DISH-ICE_CREAM", "DISH-BLUEBERRIES", "DISH-BLUEBERRIES-ICE_CREAM", "DISH")

    val nextTarget = playeritem.map(Player.nextTarget(_, customeritems, cookBook, targetStaticMap, (0,0))).zipWithIndex
    Console.err.println
    nextTarget.foreach {
      next => {
        Console.err.println(s"prev state: ${playeritem(next._2)}, target: ${next._1}")
        //        assert(next._1 == result(next._2))
      }
    }
  }
}
