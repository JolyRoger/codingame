package codingames.challenge.iceandfire.src.army

import codingames.challenge.iceandfire.src.Building

abstract class Army {
  var gold = 0
  var income = 0
  var buildings = List.empty[Building]
  var units = List.empty[Soldier]
  val headquarters: (Int, Int)
  override def toString: String =
    s"gold/income = [$gold/$income]" +
      buildings.mkString("\nbuildings: ", ", ", "") +
      units.mkString("\nunits: ", ", ", "")
  def print = Console.err.println(toString)
}