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

}
