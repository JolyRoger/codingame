package codingames.challenge.iceandfire.src.army

import codingames.challenge.iceandfire.src.World

class Scout(id: Int, level: Int, world: World, x: Int, y: Int, me: Me) extends Soldier(id, level, world, x, y) {
  override def move = super.move match {
    case None => Some((id, me.enemyHeadquarters._1, me.enemyHeadquarters._2))
    case Some(m) => Some(m)
  }
}