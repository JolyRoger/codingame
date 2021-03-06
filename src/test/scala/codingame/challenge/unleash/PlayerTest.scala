package codingame.challenge.unleash

import codingames.challenge.unleash.{Player, Robot, RobotManager}
import org.scalatest.FlatSpec

class PlayerTest extends FlatSpec {

  val robots: List[Robot] = List(
    new Robot(0, 0, 4, -1),
    new Robot(0, 0, 8, -1),
    new Robot(0, 0, 10, -1),
    new Robot(0, 0, 12, -1),
    new Robot(0, 0, 2, -1)
  )

  val mapData: List[Array[(String, Int)]] = List(
    Array(("?", 0), ("1", 1), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0)),
    Array(("?", 0), ("1", 1), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0)),
    Array(("?", 0), ("1", 1), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0)),
    Array(("?", 0), ("1", 1), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0)),
    Array(("?", 0), ("1", 1), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0)),
    Array(("?", 0), ("?", 1), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0)),
    Array(("?", 0), ("?", 1), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0)),
    Array(("?", 0), ("?", 1), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0)),
    Array(("?", 0), ("?", 1), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0)),
    Array(("?", 0), ("1", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0)),
    Array(("?", 0), ("1", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0)),
    Array(("?", 0), ("1", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0)),
    Array(("?", 0), ("1", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0)),
    Array(("?", 0), ("1", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0)),
    Array(("?", 0), ("1", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0))
  )

  val entityData: List[Array[Int]] = List(
    Array(0, 0, 0,  6, -1),
    Array(1, 0, 0,  0, -1),
    Array(2, 0, 0,  4, -1),
    Array(3, 0, 0, 12, -1),
    Array(4, 0, 0,  8, -1),
    Array(5, 1, 18, 4, -1),
    Array(6, 1, 18, 4, -1),
    Array(7, 1, 18,11, -1),
    Array(8, 1, 16, 0, -1),
    Array(9, 1, 7,  3, -1),
    Array(10, 3, 1,  5, -1)
  )

  "A Player" should "connect ore and hole to tuple" in {
    val oh = Array("?", "0", "?", "1", "?", "0", "?", "0", "?", "0", "?", "0", "?", "0", "?", "0", "?", "0", "?", "0")
    val res = Player.oreHole(oh)
    assert(res sameElements Array(
      ("?", 0),
      ("?", 1),
      ("?", 0),
      ("?", 0),
      ("?", 0),
      ("?", 0),
      ("?", 0),
      ("?", 0),
      ("?", 0),
      ("?", 0)
    ))
  }

  "A Player" should "do smth" in {
    val a = List((1,1), (1,2), (1,1), (1,3), (1,1), (1,4), (1,5))
    val b = List((1,1), (1,2), (1,3), (1,7), (1,8), (1,1))

    val res1 = a.intersect(b)
    val res2 = b.intersect(a)

    val enemyData: List[Array[Int]] = List(
      Array(1,2, 1,1, 4),
      Array(1,2, 4,2, 4),
      Array(1,2, 1,1, 4),
      Array(1,2, 9,5, 4),
      Array(1,2, 7,4, 4)
    )
    val enemyRobotsData = enemyData.groupBy(data => (data(2), data(3)))

    Console.err.println(s"$res1")
    Console.err.println(s"$res2")
    enemyRobotsData.foreach(v => Console.err.println(s"${v._1} -> ${v._2.size}"))
    Console.err.println(s"$enemyRobotsData")
  }

  "A Player" should "enrich a robot with command" in {
    Player.enrichBoardData(mapData, Player.boardData2)
    Player.calcVisibleSquares(Player.boardData2)
    Player.boardData2((1,0)).myHole = true
    Player.boardData2((1,1)).myHole = true
    robots.head.isFlying = true
    robots.head.item = 2
    RobotManager.command(robots, Player.boardData2, 1, 1, false)
  }
}
