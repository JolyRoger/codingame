package codingames.challenge.iceandfire.src.army

import codingames.challenge.iceandfire.src.World

class Conqueror(id: Int, level: Int, world: World, x: Int, y: Int) extends Soldier(id, level, world, x, y) {
  override def move = (id, x, y)
}
