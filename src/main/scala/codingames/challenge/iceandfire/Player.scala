package codingames.challenge.iceandfire

import codingames.challenge.iceandfire.src.army.{Enemy, Me, Soldier}
import codingames.challenge.iceandfire.src.{Building, Wait, World}
import scala.io.StdIn._

object Player extends App {

//------------------------------------------VARIABLES-------------------------------------------------------------------
  val condition = true
  val isDebug = false
  val output = false
  val outputLikeInput = false
  var step = 0
  val limit = 1
  val isTest = false



//------------------------------------------CLASSES---------------------------------------------------------------------
  ///Board.scala
  ///World.scala
  ///Soldier.scala
  ///Guardian.scala
  ///Scout.scala
  ///Conqueror.scala
  ///Building.scala
  ///Army.scala
  ///Me.scala
  ///Enemy.scala
  ///Action.scala
//------------------------------------------ENTERS----------------------------------------------------------------------
  val numberminespots = readInt
  val nms = (for (i <- 0 until numberminespots) yield {
    for (i <- readLine split " ") yield i.toInt
  }).toList

  if (outputLikeInput) Console.err.println(s"$numberminespots")
  if (outputLikeInput) nms.foreach(arr => Console.err.println(s"${arr(0)} ${arr(1)}"))

  val world = World(numberminespots, nms)
  val enemy = new Enemy
  val me = new Me(world)

  // game loop
  while (if (isTest) step < limit else true) {
    val gold = readInt
    if (outputLikeInput) Console.err.println(s"$gold")
    me.gold = gold
    val income = readInt
    if (outputLikeInput) Console.err.println(s"$income")
    me.income = income
    val opponentgold = readInt
    if (outputLikeInput) Console.err.println(s"$opponentgold")
    enemy.gold = opponentgold
    val opponentincome = readInt
    if (outputLikeInput) Console.err.println(s"$opponentincome")
    enemy.income = opponentincome
    world.board = (for (i <- 0 until 12) yield readLine).toArray
    world.boardMatrix = world.board.map(_.toCharArray)
    if (outputLikeInput) world.printBoard

    val buildingcount = readInt
    if (outputLikeInput) Console.err.println(s"$buildingcount")
    world.buildingcount = buildingcount

    me.buildings = List.empty
    enemy.buildings = List.empty

    for (i <- 0 until buildingcount) {
      val Array(owner, buildingtype, x, y) = for (i <- readLine split " ") yield i.toInt
      if (outputLikeInput) Console.err.println(s"$owner $buildingtype $x $y")
      val player = if (owner == 0) me else enemy
      player.buildings = Building(buildingtype, x, y) :: player.buildings
    }

    me.units = List.empty
    enemy.units = List.empty

    val unitcount = readInt
    if (outputLikeInput) Console.err.println(s"$unitcount")
    world.unitcount = unitcount
    for (i <- 0 until unitcount) {
      val Array(owner, unitid, level, x, y) = for (i <- readLine split " ") yield i.toInt
      if (outputLikeInput) Console.err.println(s"$owner $unitid $level $x $y")
      if (owner == 0) me.units = me.born(unitid, level, world, x, y) :: me.units
      else enemy.units = Soldier(unitid, level, world, x, y) :: enemy.units
      world.mySoldiers = me.units
      world.enemySoldiers = enemy.units
    }

    me.update(world)

    //------------------------------------------ACTIONS---------------------------------------------------------------------

    if (output) {
      world.print
      me.print
      enemy.print
    }

    val action = if (isDebug) List(Wait("Test")) else me.nextAction(enemy)


    step += 1
    println(s"${action.map(_.str).reduce(_ + "; " + _)}")
//    Console.err.println(board.bfs(0))
  }
}
