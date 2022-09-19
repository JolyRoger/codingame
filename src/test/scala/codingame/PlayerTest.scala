package codingame

import codingames.veryhard.voxcodei.Player
import org.scalatest.flatspec.AnyFlatSpec

class PlayerTest extends AnyFlatSpec {
  "A Player" should "run application" in {
    Player.main(Array.empty[String])
  }
}
