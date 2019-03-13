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

  "A Player" should "print target map" in {
    Console.err.println(s"${Player.stateBook}")
  }
}