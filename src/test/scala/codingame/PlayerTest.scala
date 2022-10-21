package codingame

import codingames.challenge.cultist.Player
import org.scalatest.flatspec.AnyFlatSpec

class PlayerTest extends AnyFlatSpec {
  "A Player" should "run application" in {
    Player.main(Array.empty[String])
  }
}
