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
    Array(("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0)),
    Array(("1", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0)),
    Array(("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0)),
    Array(("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0)),
    Array(("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0)),
    Array(("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0)),
    Array(("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0)),
    Array(("?", 0), ("1", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0)),
    Array(("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0)),
    Array(("?", 0), ("?", 0), ("?", 0), ("?", 0), ("2", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0)),
    Array(("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0)),
    Array(("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0)),
    Array(("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0)),
    Array(("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0)),
    Array(("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0), ("?", 0))
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
  )

  val hq: List[Int] = List(Player.toNumber((6,0)), Player.toNumber((0,0)), Player.toNumber((4,0)), Player.toNumber((12,0)), Player.toNumber((8,0)))

  "A Player" should "convert number to matrix" in {
    val playerPoint = 31
    val res = Player.toMatrix(playerPoint)
    assert(res == (1, 1))
  }

  "A Player" should "convert matrix to number" in {
    val playerPoint = (19, 2)
    val res = Player.toNumber(playerPoint)
    assert(res == 79)
  }

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

  "A Player" should "enrich a robot with command" in {
    val boardData = Player.createBoardData(30, 15)
    Player.enrichBoardData(mapData, boardData)
    val res = RobotManager.command(robots, boardData, 1, 1)
    assert(res.nonEmpty)
  }

  "A Player" should "create and populate board data" in {
    val boardData = Player.createBoardData(30, 15)
    assert(!boardData((0,0)).hole && boardData((0,0)).ore == -1)
    Player.enrichBoardData(mapData, boardData)
    assert(boardData((0,0)).hole && boardData((0,0)).ore == 2)
  }
}
