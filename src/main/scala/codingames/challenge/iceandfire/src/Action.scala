package codingames.challenge.iceandfire.src

import codingames.challenge.iceandfire.src.army.{Me, Soldier}

sealed abstract class Action {
  def str: String
}
case class Wait(message: String) extends Action {
  override def str: String = s"WAIT; MSG $message"
}
case class Train(level: Int, x: Int, y: Int) extends Action {
  override def str: String = s"TRAIN $level $x $y"
}
case class Move(me: Me) extends Action {
  val unitsMoves = me.units.map(_.move)

  override def str: String = unitsMoves.map(m => s" MOVE ${m._1} ${m._2} ${m._3};").reduce(_ + _)
}
