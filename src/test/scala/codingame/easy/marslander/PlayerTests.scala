package codingame.easy.marslander

import codingames.easy.marslander.Player._
import org.scalatest.FlatSpec

class PlayerTests extends FlatSpec {

  "A player" should "find thrust power" in {
    val gEarth = 9.81523f
    val thrust = 0
    val thrustRange = 0 to 4

    for (thr <- thrustRange) {
      Console.err.println
  //    findThrustPower(100,  2500, 0, 5001, 0)
      val timeMars = timeOfFall(2500, 100, g, thr)
      val speedMars = speedOfGround(2500, g, thr)
      val timeEarth = timeOfFall(2500, 100, gEarth, thr)
      val speedEarth = speedOfGround(2500, gEarth, thr)
      Console.err.println(s"Mars:  time of fall = $timeMars\tspeed = $speedMars\tthrust = $thr")
      Console.err.println(s"Earth: time of fall = $timeEarth\tspeed = $speedEarth\tthrust = $thr")
    }
    Console.err.println

    for (t <- 0 to 36) {
      val dist = afterSec(2500, g, t)
      Console.err.println(s"after $t sec in $dist point")
    }
  }

  "A player" should "find simple solution" in {
    val res = findFuel(100, -37, 136, 500, 0, 0)
    Console.err.println(s"FUEL: $res")
  }
}
