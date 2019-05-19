package codingames.challenge.iceandfire.src

sealed abstract class Action {
  def str: String
}
case class Wait(message: String) extends Action {
  override def str: String = s"WAIT; MSG $message"
}
case class Train(level: Int, x: Int, y: Int) extends Action {
  override def str: String = s"TRAIN $level $x $y"
}
case class Move(id: Int, x: Int, y: Int) extends Action {
  override def str: String = s"MOVE $id $x $y"
}
