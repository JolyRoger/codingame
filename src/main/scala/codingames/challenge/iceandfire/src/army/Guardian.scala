package codingames.challenge.iceandfire.src.army

import codingames.challenge.iceandfire.src.World

class Guardian(id: Int, level: Int, world: World, x: Int, y: Int,
               headquarters: (Int, Int)) extends Soldier(id, level, world, x, y) {
  override def move = ???
}
