package codingame.veryhard.knight

import codingames.veryhard.knight.Player
import org.scalatest.flatspec.AnyFlatSpec

class KnightRunner extends AnyFlatSpec {
  "Player" should "run the application" in {
    Player.main(Array.empty[String])
  }
}
