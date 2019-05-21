package codingames.challenge.iceandfire.src.army

import codingames.challenge.iceandfire.src.World

class Guardian(id: Int, level: Int, world: World, x: Int, y: Int, me: Me) extends Soldier(id, level, world, x, y) {
  override def move = super.move match {
    case None =>
      val hqFree = world.closestFree(me.headquarters._1, me.headquarters._2)
      if (hqFree.nonEmpty) Some((id, hqFree.head._1, hqFree.head._2))
      else {
//        me.turnTo(id, "Scout")
        Some((id, x, y))
      }
    case Some(m) => Some(m)
  }
}
