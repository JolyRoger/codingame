package codingame.hard.skynet

import codingames.hard.skynet.Player
import codingames.hard.skynet.Player.{Graph, n}
import org.scalatest.flatspec.AnyFlatSpec

class PlayerTests extends AnyFlatSpec {

  "A Player" should "run application" in {
    Player.main(Array.empty[String])
  }

  "A Player" should "find all exit neighbours" in {
    val graph = new Graph(37)
    Player.main(Array.empty[String])
  }
}