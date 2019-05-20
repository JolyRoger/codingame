package codingames.challenge.iceandfire.src.army

import codingames.challenge.iceandfire.src.World

class Scout(id: Int, level: Int, world: World, x: Int, y: Int, enemyHQ: (Int, Int)) extends Soldier(id, level, world, x, y) {
  override def move = (id, enemyHQ._1, enemyHQ._2)
}