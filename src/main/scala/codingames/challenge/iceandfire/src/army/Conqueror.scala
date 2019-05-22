package codingames.challenge.iceandfire.src.army

import codingames.challenge.iceandfire.src.World

class Conqueror(id: Int, level: Int, world: World, x: Int, y: Int, me: Me) extends Soldier(id, level, world, x, y) {
  override def move = super.move match {
    case None =>
      val goal = world.closestForConqueror(x, y).headOption.getOrElse((me.enemyHeadquarters._1, me.enemyHeadquarters._2))
      Some((id, goal._1, goal._2))
    case Some(m) => Some(m)
  }
}
