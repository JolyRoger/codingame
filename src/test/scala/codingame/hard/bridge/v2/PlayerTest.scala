package codingame.hard.bridge.v2

import codingames.hard.bridge.v2.Player
import org.scalatest.FlatSpec

class PlayerTests extends FlatSpec {
  "A Player" should "calc" in {
    val order = Map(0 -> "SPEED", 1 -> "JUMP", 2 -> "WAIT", 3 -> "UP", 4 -> "DOWN", 5 -> "SLOW")
    val stack = Player.calc(0, 2, 1)
    Console.err.println
    stack.foreach(command => Console.err.println(s"${order(command._1)} "))
  }
}
