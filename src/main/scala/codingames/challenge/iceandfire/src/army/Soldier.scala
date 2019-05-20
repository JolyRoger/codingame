package codingames.challenge.iceandfire.src.army

import codingames.challenge.iceandfire.src.World

case class Soldier(id: Int, var level: Int, world: World, x: Int, y: Int) {
  def closestEmpty = world.closestEmpty(x, y)
  def move: (Int, Int, Int) = (id, x, y)
}