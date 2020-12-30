package codingame.challenge.platinum

import codingames.challenge.platinum.Player
import org.scalatest.FlatSpec

class PlatinumTest extends FlatSpec {

  "Player" should "run the application" in {
    Player.main(Array.empty[String])
  }

  "A divider" should "split a divide for equal parts" in {
    val res1 = Player.smartDivide(10, 3)
    val res2 = Player.smartDivide(11, 3)
    val res3 = Player.smartDivide(15, 5)
    assert(res1.sorted === List(3, 4, 3).sorted)
    assert(res2.sorted === List(3, 4, 4).sorted)
    assert(res3 === List(3, 3, 3, 3, 3))
  }
}
