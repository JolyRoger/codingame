package codingame.hard

import codingames.hard.voxcodei.Player
import org.scalatest.flatspec.AnyFlatSpec
import scala.io.Source

class PlayerTest extends AnyFlatSpec {
  "A Player" should "run application" in {
    Player.main(Array.empty[String])
  }
}
