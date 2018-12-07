package codingame.challenge.xmasrush

import codingames.challenge.xmasrush.Player
import org.scalatest.FlatSpec

class PlayerTests extends FlatSpec {
  "A Player" should "return closest point" in {
    val playerPoint = (5, 1)
    val distance = List((2, 2), (3, 4), (5, 0))
    val distance1 = Player.euclidean(playerPoint, distance(0))
    val distance2 = Player.euclidean(playerPoint, distance(1))
    val distance3 = Player.euclidean(playerPoint, distance(2))
    println(s"distance1=$distance1\tdistance2=$distance2\tdistance3=$distance3")
    val closestPoint = Player.closest(playerPoint, distance)
    println(s"closest=$closestPoint")
    assert(closestPoint === (5, 0))
  }
}
