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

case class Build(buildType: String, x: Int, y: Int) extends Action {
  override def str: String = s"BUILD $buildType $x $y"
}

case class Move(me: Me) extends Action {
  val unitsMoves = me.units.map(_.move)

  override def str: String = unitsMoves.map(m => s" MOVE ${m.get._1} ${m.get._2} ${m.get._3}").reduce(_ + "; " + _)
}
