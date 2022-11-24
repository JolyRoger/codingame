package codingame

import codingames.veryhard.knight.{Player, Player0, Player2, Player3}
import org.scalatest.flatspec.AnyFlatSpec

class PlayerTest extends AnyFlatSpec {
  "A Player" should "run application" in {
    Player3.main(Array.empty[String])
  }
}
