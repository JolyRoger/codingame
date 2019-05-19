package codingames.challenge.iceandfire.src.army

import codingames.challenge.iceandfire.src.World

class Guardian(id: Int, override var level: Int, world: World, override var x: Int, override var y: Int,
               headquarters: (Int, Int)) extends Soldier(id, level, world, x, y) {
  override def move = ???
}
