package codingame.challenge.codealamode

import codingames.challenge.codealamode.Player
import codingames.challenge.codealamode.Player.searchSym
import org.scalatest.FlatSpec

class PlayerTests extends FlatSpec {
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
  
  "A Player" should "breadth-first search" in {
    val g = Player.createGraph(testMatrix)
    val bfs = g.bfs(Player.toNumber((1, 1)))
    Console.err.println(s"$bfs")
  }

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
  "A Player" should "find another target" in {
    val customeritems = List("DISH-ICE_CREAM-BLUEBERRIES", "DISH-ICE_CREAM-BLUEBERRIES", "DISH-ICE_CREAM-BLUEBERRIES")
    val playeritem = "DISH-ICE_CREAM"
    val nextTarget = Player.nextTarget(playeritem, customeritems)
    Console.err.println(s"nextTarget: $nextTarget")
    assert(nextTarget == List("BLUEBERRIES"))
  }

  "A Player" should "find next target" in {
    val customeritems = List("DISH-CHOPPED_STRAWBERRIES-BLUEBERRIES", "DISH-ICE_CREAM-BLUEBERRIES", "DISH-ICE_CREAM-CHOPPED_STRAWBERRIES")
//    val customeritemsA = List("DISH-BLUEBERRIES-ICE_CREAM", "DISH-ICE_CREAM-BLUEBERRIES", "DISH-ICE_CREAM-BLUEBERRIES")
    Console.err.println(s"customer items: $customeritems")
    val result = List(List("DISH"), List("BLUEBERRIES"), List("ICE_CREAM"), List("WINDOW"), List("BLUEBERRIES", "ICE_CREAM"))
    val playeritem = List("NONE", "DISH-ICE_CREAM", "DISH-BLUEBERRIES", "DISH-BLUEBERRIES-ICE_CREAM", "DISH")

    val nextTarget = playeritem.map(Player.nextTarget(_, customeritems)).zipWithIndex
    Console.err.println
    nextTarget.foreach {
      next => {
        Console.err.println(s"prev state: ${playeritem(next._2)}, target: ${next._1}")
//        assert(next._1 == result(next._2))
      }
    }
  }
  
  "A Player" should "reach target" in {
    val playeritem = "NONE"
    val customeritems = List("DISH-BLUEBERRIES-ICE_CREAM", "DISH-ICE_CREAM-BLUEBERRIES", "DISH-ICE_CREAM-BLUEBERRIES")
    val nextTarget = Player.nextTarget(playeritem, customeritems)
    val g = Player.createGraph(testMatrix)
    val player = Player.searchSym(testMatrix, '0').getOrElse((-1,-1))
    val res = Player.reach(g, testMatrix, player, nextTarget.map(targetMap.apply))
    Console.err.println(s"res: $res")
  }
}
