package codingames.challenge.iceandfire.src.army

import codingames.challenge.iceandfire.src.World

case class Soldier(id: Int, var level: Int, world: World, x: Int, y: Int) {
  def canKill(myLevel: Int, enemyLevel: Int) = myLevel > enemyLevel || (myLevel == 3 && enemyLevel == 3)
  def findCanKill(enemyList: List[((Int, Int), Soldier)]) = enemyList.find(enemySoldier => canKill(level, enemySoldier._2.level))
  def closestEmpty = world.closestEmpty(x, y)
  def move: Option[(Int, Int, Int)] = {
    val target = findCanKill(world.closestEnemySoldiers(x ,y))
    if (target.isDefined) {
      Some((id, target.get._1._1, target.get._1._2))
    } else None
  }

  override def toString = s"$level:[$x,$y]"
}