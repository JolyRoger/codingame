package codingames.challenge.iceandfire.src.army

import codingames.challenge.iceandfire.src.World

abstract case class Soldier(id: Int, var level: Int, world: World, var x: Int, var y: Int) {
  def closestEmpty = world.closestEmpty(x, y)
  def move: (Int, Int)
}