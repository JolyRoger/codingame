package codingame.challenge.unleash

import codingames.challenge.unleash.Player
import org.scalatest.FlatSpec

class PlayerTest extends FlatSpec {

  "A Player" should "convert number to matrix" in {
    val playerPoint = 31
    val res = Player.toMatrix(playerPoint)
    assert(res == (1, 1))
  }

  "A Player" should "convert matrix to number" in {
    val playerPoint = (19, 2)
    val res = Player.toNumber(playerPoint)
    assert(res == 79)
  }

  "A Player" should "connect ore and hole to tuple" in {
    val oh = Array("?", "0", "?", "1", "?", "0", "?", "0", "?", "0", "?", "0", "?", "0", "?", "0", "?", "0", "?", "0")
    val res = Player.oreHole(oh)
    assert(res sameElements Array(
      ("?", "0"),
      ("?", "1"),
      ("?", "0"),
      ("?", "0"),
      ("?", "0"),
      ("?", "0"),
      ("?", "0"),
      ("?", "0"),
      ("?", "0"),
      ("?", "0")
    ))
  }
}
