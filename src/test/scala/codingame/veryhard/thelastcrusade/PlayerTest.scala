package codingame.veryhard.thelastcrusade

import codingames.veryhard.thelastcrusade.Player
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.immutable.{ListSet, SortedSet, TreeSet}

class PlayerTest extends AnyFlatSpec {

  "A Player" should "run the application" in {
    Player.main(Array.empty[String])
  }

  "A Player" should "check a ListSet" in {
    val a = ListSet(1,3,2,5,3,6,8,4,3)
    val b = TreeSet(1,3,2,5,3,6,8,4,3)
    val c = SortedSet(1,3,2,5,3,6,8,4,3)
    Console.err.println(s"$a")
    Console.err.println(s"$b")
    Console.err.println(s"$c")

    Console.err.println(s"${a(2)}")
    Console.err.println(s"${a.head}")
  }

}
