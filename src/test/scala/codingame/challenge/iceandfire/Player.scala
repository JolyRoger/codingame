package codingame.challenge.iceandfire

import java.io.File

import math._
import scala.io.Source
import scala.util._

/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/
object Player extends App {
//------------------------------------------VARIABLES-------------------------------------------------------------------
  val limit = 2
  var step = 0
  val isTest = true
  val isDebug = false
  val output = true
  val outputLikeInput = false
//------------------------------------------FILE ENTRY------------------------------------------------------------------
  val filename = "iceandfire0.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines

  def readInt = if (data.hasNext) data.next.toInt else -1
  def readLine = if (data.hasNext) data.next else "\0"

//------------------------------------------CLASSES---------------------------------------------------------------------

  case class World(numMineSpots: Int, mineSpotsData: List[Array[Int]]) {                                                        // World class
    var buildingcount = 0
    var unitcount = 0
    var board = Array.empty[String]
    var boardMatrix: Array[Array[Char]] = Array.empty
    def closestWithoutUnits(x: Int, y: Int, units: List[Unit], sym: List[Char]) =
      closest(x, y, sym).filterNot(
        point => units.exists(
          unit => unit.x == point._1 && unit.y == point._2))
    def closest(x: Int, y: Int, sym: List[Char]) = {
      val candidates = List((x-1, y), (x, y-1), (x+1, y), (x, y+1))
      candidates.filter(square =>
        square._1 >= 0 &&
        square._2 >= 0 &&
        square._1 < 12 &&
        square._2 < 12 &&
        sym.contains(boardMatrix(square._2)(square._1)))
    }
    def printBoard = board.foreach(Console.err.println)
    def print = {
      Console.err.println(s"numMineSpots=$numMineSpots")
      mineSpotsData.foreach(ms => Console.err.println(s"\t${ms(0)} ${ms(1)}"))
      Console.err.println(s"buildingcount=$buildingcount")
      Console.err.println(s"unitcount=$unitcount")
      printBoard
    }
  }
  case class Unit(id: Int, var level: Int, var x: Int, var y: Int)
  case class Building(btype: Int, x: Int, y: Int)

  sealed abstract class Army {
    var gold = 0
    var income = 0
    var buildings = List.empty[Building]
    var units = List.empty[Unit]
    val headquarters: (Int, Int)
    override def toString: String =
      s"gold/income = [$gold/$income]" +
        buildings.mkString("\nbuildings: ", ", ", "") +
        units.mkString("\nunits: ", ", ", "")
    def print = Console.err.println(toString)
  }

  class Me extends Army {                                                                                                       // Me class
    override lazy val headquarters: (Int, Int) = (buildings.head.x, buildings.head.y)
    private var hqClosest: List[(Int, Int)] = List.empty
    private var trainLevel: Int = 0

    def trainCondition= units.isEmpty ||
      (gold > 50 && hqClosest.nonEmpty)

    def nextAction = if (trainCondition) {
      val hqc = hqClosest.head
      Train(trainLevel, hqc._1, hqc._2)
    } else {
      //      val unitsMove = me.units.map(unit => me.closest(unit).orElse())
      val enemyHQ = enemy.headquarters
      Move(1, enemyHQ._1, enemyHQ._2)
    }

    override def toString: String = "ME-------------------------------------------------------------\n" + super.toString

    def closest(unit: Unit) = world.closest(unit.x, unit.y, List('.'))

    def update = {
      hqClosest = world.closestWithoutUnits(headquarters._1, headquarters._2, units, List('.', 'O'))
      trainLevel = if (gold < 50) 1 else 2
    }
  }

  class Enemy extends Army {
    //    override lazy val headquarters: (Int, Int) = searchSym(world.board, 'X').get
    override lazy val headquarters: (Int, Int) = (buildings.head.x, buildings.head.y)
    override def toString: String = "ENEMY----------------------------------------------------------\n" + super.toString
  }

  sealed abstract class Action {
    def str: String
  }
  case class Wait(message: String) extends Action {
    override def str: String = s"WAIT; MSG $message"
  }
  case class Train(level: Int, x: Int, y: Int) extends Action {
    override def str: String = s"TRAIN $level $x $y"
  }
  case class Move(id: Int, x: Int, y: Int) extends Action {
    override def str: String = s"MOVE $id $x $y"
  }

  //------------------------------------------FUNCTIONS-------------------------------------------------------------------

  def searchSym(matrix: Array[String], sym: Char) = {
    val coords = for (row <- matrix.indices; col <- matrix(row).indices; _sym = matrix(row)(col); if _sym == sym) yield (col, row)
    if (coords.nonEmpty) Some(coords(0)) else None
  }

  //------------------------------------------ENTERS----------------------------------------------------------------------
  val numberminespots = readInt
  val nms = (for (i <- 0 until numberminespots) yield {
    for (i <- readLine split " ") yield i.toInt
  }).toList

  if (outputLikeInput) Console.err.println(s"$numberminespots")
  if (outputLikeInput) nms.foreach(arr => Console.err.println(s"${arr(0)} ${arr(1)}"))

  val world = World(numberminespots, nms)


  val me = new Me
  val enemy = new Enemy

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
      val player = if (owner == 0) me else enemy
      player.units = Unit(unitid, level, x, y) :: player.units
    }
    me.update

    //------------------------------------------ACTIONS---------------------------------------------------------------------

    if (output) {
      world.print
      me.print
      enemy.print
    }

    val action = if (isDebug) Wait("Test") else me.nextAction


    step += 1
    println(s"${action.str}")
    Console.err.println
  }
}