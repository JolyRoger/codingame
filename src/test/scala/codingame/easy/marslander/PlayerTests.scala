package codingame.easy.marslander

import codingames.easy.marslander.Player._
import org.scalatest.FlatSpec

class PlayerTests extends FlatSpec {
  "A player" should "find thrust power" in {
    findThrustPower(100,  2500, 0, 5001, 0)
  }
}
