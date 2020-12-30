package codingame.hard.bridge.v2

import codingames.hard.bridge.v2.Player
import org.scalatest.flatspec.AnyFlatSpec

class PlayerTests extends AnyFlatSpec {


  /*val*/ def linesOrig = Array("................000000000........00000........000.............00.",
                                "...0..................000....000......0.0..................00000.",
                                "....000.........0.0...000................000............000000.0.",
                                "............0.000000...........0000...............0.0.....000000.")
//  "A Player" should "calc" in {
//    val order = Map(0 -> "SPEED", 1 -> "JUMP", 2 -> "WAIT", 3 -> "UP", 4 -> "DOWN", 5 -> "SLOW")
//    val stack = Player.calc(0, 2, 1)
//    Console.err.println
//    stack.foreach(command => Console.err.println(s"${order(command._1)} "))
//  }

  "A Player" should "calc" in {
    val init = List(Array(0, 0, 1, 3), Array(0, 1, 1, 3), Array(0, 2, 1, 3), Array(0, 3, 1, 3))
    val stack = Player.calc(init, 1)
    stack.foreach(command => Console.err.println(s"${Player.order(command._1)} "))
  }

  "A Player" should "find new x y s" in {
    val init = List(Array(0, 0, 1, 6), Array(0, 1, 1, 6), Array(0, 2, 1, 6), Array(0, 3, 1, 6))
    val res = Player.newXYS((0, init))
    res.foreach(r => Console.err.println(s"(${r._1.mkString("Array(", ", ", ")")}, ${r._2.map(_.mkString("Array(", ", ", ")"))})"))
  }
  "A Player" should "check if is active" in {
    val first = (0, List(Array(0, 0, 1, 6), Array(0, 1, 1, 6), Array(0, 2, 1, 6), Array(0, 3, 1, 6)))
    val second = (0, List(Array(7, 0, 1, 7), Array(7, 1, 1, 7), Array(7, 2, 1, 7), Array(7, 3, 1, 7)))
    val stack = List(first, second)
    val res = Player.isActive(stack)
    Console.err.println(s"res: $res")
  }
  "A Player" should "check if point is valid" in {
    val oldXYS = List( Array(0, 0, 1, 6), Array(0, 1, 1, 6), Array(0, 2, 1, 6), Array(0, 3, 1, 6))
    val newXYS = List((Array(7, 0, 1, 7), List(Array(0, 0), Array(1, 0), Array(2, 0), Array(3, 0), Array(4, 0), Array(5, 0), Array(6, 0), Array(7, 0))),
                      (Array(7, 1, 1, 7), List(Array(0, 1), Array(1, 1), Array(2, 1), Array(3, 1), Array(4, 1), Array(5, 1), Array(6, 1), Array(7, 1))),
                      (Array(7, 2, 1, 7), List(Array(0, 2), Array(1, 2), Array(2, 2), Array(3, 2), Array(4, 2), Array(5, 2), Array(6, 2), Array(7, 2))),
                      (Array(7, 3, 1, 7), List(Array(0, 3), Array(1, 3), Array(2, 3), Array(3, 3), Array(4, 3), Array(5, 3), Array(6, 3), Array(7, 3))))

    val yeswecan = Player.can(newXYS, oldXYS, Set.empty[String], 3)
    Console.err.println(s"yeswecan=$yeswecan")
  }

  "A Player" should "test for contain" in {
    val item = List((Array(7, 0, 1, 7), List(Array(0, 0), Array(1, 0), Array(2, 0), Array(3, 0), Array(4, 0), Array(5, 0), Array(6, 0), Array(7, 0))),
                    (Array(7, 1, 1, 7), List(Array(0, 1), Array(1, 1), Array(2, 1), Array(3, 1), Array(4, 1), Array(5, 1), Array(6, 1), Array(7, 1))),
                    (Array(7, 2, 1, 7), List(Array(0, 2), Array(1, 2), Array(2, 2), Array(3, 2), Array(4, 2), Array(5, 2), Array(6, 2), Array(7, 2))),
                    (Array(7, 3, 1, 7), List(Array(0, 3), Array(1, 3), Array(2, 3), Array(3, 3), Array(4, 3), Array(5, 3), Array(6, 3), Array(7, 3)))).map(_._1)

    val theSameItem = List((Array(7, 0, 1, 7), List(Array(0, 0), Array(1, 0), Array(2, 0), Array(3, 0), Array(4, 0), Array(5, 0), Array(6, 0), Array(7, 0))),
      (Array(7, 1, 1, 7), List(Array(0, 1), Array(1, 1), Array(2, 1), Array(3, 1), Array(4, 1), Array(5, 1), Array(6, 1), Array(7, 1))),
      (Array(7, 2, 1, 7), List(Array(0, 2), Array(1, 2), Array(2, 2), Array(3, 2), Array(4, 2), Array(5, 2), Array(6, 2), Array(7, 2))),
      (Array(7, 3, 1, 7), List(Array(0, 3), Array(1, 3), Array(2, 3), Array(3, 3), Array(4, 3), Array(5, 3), Array(6, 3), Array(7, 3)))).map(_._1)

    val pointSet = Set(Player.string(item))

    val res = Player.notContain(pointSet, theSameItem)

    Console.err.println(s"res: $res")
  }

  "A Player" should "check for succesfull" in {
    val item = (0, List(Array(50, 0, 1, 3), Array(50, 1, 1, 3), Array(76, 2, 1, 3), Array(2, 3, 0, 3)))
    val res = Player.successful(item)
    Console.err.println(s"res: $res")
  }

  "A Player" should "transform new coords" in {
    val newXYS = List((Array(7, 0, 1, 7), List(Array(0, 0), Array(1, 0), Array(2, 0), Array(3, 0), Array(4, 0), Array(5, 0), Array(6, 0), Array(7, 0))),
                      (Array(7, 1, 1, 7), List(Array(0, 1), Array(1, 1), Array(2, 1), Array(3, 1), Array(4, 1), Array(5, 1), Array(6, 1), Array(7, 1))),
                      (Array(7, 2, 1, 7), List(Array(0, 2), Array(1, 2), Array(2, 2), Array(3, 2), Array(4, 2), Array(5, 2), Array(6, 2), Array(7, 2))),
                      (Array(7, 3, 1, 7), List(Array(0, 3), Array(1, 3), Array(2, 3), Array(3, 3), Array(4, 3), Array(5, 3), Array(6, 3), Array(7, 3))))
    Console.err.println(s"before")
    Console.err.println(s"(${newXYS.head._1.mkString("Array(", ", ", ")")}, ${newXYS.head._2.map(_.mkString("Array(", ", ", ")"))})")
    Console.err.println(s"(${newXYS.last._1.mkString("Array(", ", ", ")")}, ${newXYS.last._2.map(_.mkString("Array(", ", ", ")"))})")
    val transformedCoordsHead = Player.survive(newXYS.head)
    val transformedCoordsLast = Player.survive(newXYS.last)
    Console.err.println(s"after")
    Console.err.println(s"(${transformedCoordsHead._1.mkString("Array(", ", ", ")")}, ${transformedCoordsHead._2.map(_.mkString("Array(", ", ", ")"))})")
    Console.err.println(s"(${transformedCoordsLast._1.mkString("Array(", ", ", ")")}, ${transformedCoordsLast._2.map(_.mkString("Array(", ", ", ")"))})")
  }
}
