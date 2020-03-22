package codingame.challenge.ocean

import codingames.challenge.ocean.Player
import org.scalatest.FlatSpec

class PlayerTest extends FlatSpec {

  "A Player" should "run application" in {
    Player.main(Array.empty[String])
  }

}
