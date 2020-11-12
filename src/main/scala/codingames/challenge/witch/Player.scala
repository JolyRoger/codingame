package codingames.challenge.witch

import codingames.challenge.witch.Player.actionTypeMap

import math._
import scala.io.Source
import scala.util._
import scala.io.StdIn._

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Player extends App {

//------------------------------------------FILE ENTRY------------------------------------------------------------------
                                                          val filename = "resources/witch/witch1.txt"
                                                          val bufferedSource = Source.fromFile(filename)
                                                          val data = bufferedSource.getLines
                                                          def readInt = if (data.hasNext) data.next.toInt else -1
                                                          def readLine = if (data.hasNext) data.next else "EOF"
//----------------------------------------------------------------------------------------------------------------------

  object SpellType extends Enumeration {
    type SpellType = Value
    val BREW, CAST, OPPCAST, LEARN = Value
  }
  import SpellType._

  val actionTypeMap = Map("BREW" -> SpellType.BREW, "CAST" -> SpellType.CAST, "OPPONENT_CAST" -> SpellType.OPPCAST)

  sealed abstract class Spell(val id: Int, val spellType: SpellType, val delta: Array[Int], val price: Int, val tomeIndex: Int,
                       val taxCount: Int, val castable: Boolean, val repeatable: Boolean) {}
  case class Brew(override val id: Int, override val spellType: SpellType, override val delta: Array[Int], override val price: Int, override val tomeIndex: Int,
                  override val taxCount: Int, override val castable: Boolean, override val repeatable: Boolean) extends Spell(id, spellType, delta, price, tomeIndex, taxCount, castable, repeatable) {}
  case class Cast(override val id: Int, override val spellType: SpellType, override val delta: Array[Int], override val price: Int, override val tomeIndex: Int,
             override val taxCount: Int, override val castable: Boolean, override val repeatable: Boolean) extends Spell(id, spellType, delta, price, tomeIndex, taxCount, castable, repeatable) {}
  case class OppCast(override val id: Int, override val spellType: SpellType, override val delta: Array[Int], override val price: Int, override val tomeIndex: Int,
                override val taxCount: Int, override val castable: Boolean, override val repeatable: Boolean) extends Spell(id, spellType, delta, price, tomeIndex, taxCount, castable, repeatable) {}

  var allSpells: Map[Int, Spell] = Map.empty

  def createSpell(id: Int, spellType: SpellType, delta: Array[Int], price: Int, tomeIndex: Int, taxCount: Int, castable: Boolean, repeatable: Boolean): Spell = {
    spellType match {
      case SpellType.BREW => Brew(id, spellType, delta, price, tomeIndex, taxCount, castable, repeatable)
      case SpellType.CAST => Cast(id, spellType, delta, price, tomeIndex, taxCount, castable, repeatable)
      case SpellType.OPPCAST => OppCast(id, spellType, delta, price, tomeIndex, taxCount, castable, repeatable)
      case _ => throw new IllegalArgumentException("Unknown Spell type")
    }
  }

  // game loop
  while (true) {
    val actionCount = readLine.toInt // the number of spells and recipes in play
    System.err.println(s"$actionCount")
    var id = 0
    allSpells = Map.empty

    for (i <- 0 until actionCount) {
      // actionId: the unique ID of this spell or recipe
      // actionType: in the first league: BREW; later: CAST, OPPONENT_CAST, LEARN, BREW
      // delta0: tier-0 ingredient change
      // delta1: tier-1 ingredient change
      // delta2: tier-2 ingredient change
      // delta3: tier-3 ingredient change
      // price: the price in rupees if this is a potion
      // tomeIndex: in the first two leagues: always 0; later: the index in the tome if this is a tome spell, equal to the read-ahead tax
      // taxCount: in the first two leagues: always 0; later: the amount of taxed tier-0 ingredients you gain from learning this spell
      // castable: in the first league: always 0; later: 1 if this is a castable player spell
      // repeatable: for the first two leagues: always 0; later: 1 if this is a repeatable player spell
      val Array(_actionId, actionType, _delta0, _delta1, _delta2, _delta3, _price, _tomeIndex, _taxCount, _castable, _repeatable) = readLine split " "
      System.err.println(s"${_actionId}, $actionType, ${_delta0}, ${_delta1}, ${_delta2}, ${_delta3}, ${_price}, ${_tomeIndex}, ${_taxCount}, ${_castable}, ${_repeatable}")
      val actionId = _actionId.toInt
      val delta0 = _delta0.toInt
      val delta1 = _delta1.toInt
      val delta2 = _delta2.toInt
      val delta3 = _delta3.toInt
      val price = _price.toInt
      val tomeIndex = _tomeIndex.toInt
      val taxCount = _taxCount.toInt
      val castable = _castable.toInt != 0
      val repeatable = _repeatable.toInt != 0

      allSpells += (actionId -> createSpell(actionId, actionTypeMap(actionType), Array(delta0, delta1, delta2, delta3), price, tomeIndex, taxCount, castable, repeatable))
/*
      allSpells = allSpells.get(actionId) match {
          case None =>
            allSpells + (actionId -> createSpell(actionId, actionTypeMap(actionType), Array(delta0, delta1, delta2, delta3), price, tomeIndex, taxCount, castable, repeatable))
          case Some(spell) =>
            spell.delta = Array(delta0, delta1, delta2, delta3)
            spell.price = price
            spell.tomeIndex = tomeIndex
            spell.taxCount = taxCount
            spell.castable = castable
            spell.repeatable = repeatable
            allSpells
      }
*/


      if (actionType == "BREW") id = actionId
    }
    for (i <- 0 until 2) {
      // inv0: tier-0 ingredients in inventory
      // score: amount of rupees
      val Array(inv0, inv1, inv2, inv3, score) = (readLine split " ").map(_.toInt)
      System.err.println(s"\t$inv0 $inv1 $inv2 $inv3 $score")
    }

    // Write an action using println
    // To debug: Console.err.println("Debug messages...")


    // in the first league: BREW <id> | WAIT; later: BREW <id> | CAST <id> [<times>] | LEARN <id> | REST | WAIT
    println(s"BREW $id")
  }
}