package codingames.challenge.iceandfire.src.army

class Enemy extends Army {
  //    override lazy val headquarters: (Int, Int) = searchSym(world.board, 'X').get
  override lazy val headquarters: (Int, Int) = (buildings.head.x, buildings.head.y)
  override def toString: String = "ENEMY----------------------------------------------------------\n" + super.toString
}
