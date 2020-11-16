package codingames.challenge.witch

import codingames.challenge.witch.Player.{Spell, me}

import math._
import scala.collection.mutable
import scala.collection.mutable.List
import scala.io.Source
import scala.util._
import scala.io.StdIn._

object Player2 extends App {

  //------------------------------------------FILE ENTRY------------------------------------------------------------------
  //                                                          val filename = "resources/witch/witch1.txt"
  //                                                          val bufferedSource = Source.fromFile(filename)
  //                                                          val data = bufferedSource.getLines
  //                                                          def readInt = if (data.hasNext) data.next.toInt else -1
  //                                                          def readLine = if (data.hasNext) data.next else "EOF"
  //----------------------------------------------------------------------------------------------------------------------

  type SpellFilter = List[Spell] => List[Spell]
  object SpellType extends Enumeration {
    type SpellType = Value
    val BREW, CAST, OPPCAST, LEARN = Value
  }
  import SpellType._

  var next: Option[String] = None
  val actionTypeMap = Map("BREW" -> SpellType.BREW, "CAST" -> SpellType.CAST, "OPPONENT_CAST" -> SpellType.OPPCAST,  "LEARN" -> SpellType.LEARN)
  
    
  case class EstimationData(negativeSum: Int, level: Int, root: String)
  sealed abstract class Pers(val inv: Array[Int], val score: Int)
  case class Me(override val inv: Array[Int], override val score: Int) extends Pers(inv, score)
  case class Opp(override val inv: Array[Int], override val score: Int) extends Pers(inv, score)
  sealed abstract class Spell(val id: Int, val spellType: SpellType, val delta: Array[Int], val price: Int, val tomeIndex: Int, val taxCount: Int, val castable: Boolean, val repeatable: Boolean) {
    def getCommand = s"${spellType.toString} $id"
    def sumWithInv(inv: Array[Int]): Array[Int] = delta.zipWithIndex.map(d => d._1 + inv(d._2))
    def currentSum: Int = delta.sum
    def totalSum(inv: Array[Int]): Int = sumWithInv(inv).sum
  }
  case class Brew(override val id: Int, override val spellType: SpellType, override val delta: Array[Int], override val price: Int, override val tomeIndex: Int, override val taxCount: Int, override val castable: Boolean, override val repeatable: Boolean) extends Spell(id, spellType, delta, price, tomeIndex, taxCount, castable, repeatable) {
    override def sumWithInv(inv: Array[Int]): Array[Int] = delta.zipWithIndex.filter(_._1 < 0).map(d => d._1 + inv(d._2))
    override def currentSum: Int = delta.filter(_ < 0).sum
    override def totalSum(inv: Array[Int]): Int = sumWithInv(inv).filter(_ < 0).sum
  }
  case class Cast(override val id: Int, override val spellType: SpellType, override val delta: Array[Int], override val price: Int, override val tomeIndex: Int, override val taxCount: Int, override val castable: Boolean, override val repeatable: Boolean) extends Spell(id, spellType, delta, price, tomeIndex, taxCount, castable, repeatable) {}
  case class OppCast(override val id: Int, override val spellType: SpellType, override val delta: Array[Int], override val price: Int, override val tomeIndex: Int, override val taxCount: Int, override val castable: Boolean, override val repeatable: Boolean) extends Spell(id, spellType, delta, price, tomeIndex, taxCount, castable, repeatable) {}
  case class Learn(override val id: Int, override val spellType: SpellType, override val delta: Array[Int], override val price: Int, override val tomeIndex: Int, override val taxCount: Int, override val castable: Boolean, override val repeatable: Boolean) extends Spell(id, spellType, delta, price, tomeIndex, taxCount, castable, repeatable) {}
  case class Node(root: Spell, spell: Spell, newInv: Array[Int], parent: Node, branch: List[Node]) {
    override def toString: String = s"${root.getCommand}, newInv=[${newInv.mkString(",")}], parent is${if (parent == null) "" else " NOT"} null, branchSize=${branch.size}"
  }

  val estimation: Map[Int, EstimationData] = Map.empty  
  var allSpells: Map[Int, Spell] = Map.empty
  var me: Me = _
  var opp: Opp = _

  def createSpell(id: Int, spellType: SpellType, delta: Array[Int], price: Int, tomeIndex: Int, taxCount: Int, castable: Boolean, repeatable: Boolean): Spell = {
    spellType match {
      case SpellType.BREW => Brew(id, spellType, delta, price, tomeIndex, taxCount, castable, repeatable)
      case SpellType.CAST => Cast(id, spellType, delta, price, tomeIndex, taxCount, castable, repeatable)
      case SpellType.OPPCAST => OppCast(id, spellType, delta, price, tomeIndex, taxCount, castable, repeatable)
      case SpellType.LEARN => Learn(id, spellType, delta, price, tomeIndex, taxCount, castable, repeatable)
      case _ => throw new IllegalArgumentException("Unknown Spell type")
    }
  }

  def readData(): Unit = {
    val actionCount = readLine.toInt // the number of spells and recipes in play
    System.err.println(s"$actionCount")
    allSpells = Map.empty

    System.err.println(s"ID\tType\td0\td1\td2\td3\tPrice\tTome\tTax\tCast\tRep")
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
      System.err.println(s"${_actionId}\t$actionType\t${_delta0}\t${_delta1}\t${_delta2}\t${_delta3}\t${_price}\t${_tomeIndex}\t${_taxCount}\t${_castable}\t${_repeatable}")
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
    }
    // inv0: tier-0 ingredients in inventory
    // score: amount of rupees
    val Array(inv0Me, inv1Me, inv2Me, inv3Me, scoreMe) = (readLine split " ").map(_.toInt)
    me = Me(Array(inv0Me, inv1Me, inv2Me, inv3Me), scoreMe)
    System.err.println(s"\t$inv0Me $inv1Me $inv2Me $inv3Me $scoreMe")
    val Array(inv0Opp, inv1Opp, inv2Opp, inv3Opp, scoreOpp) = (readLine split " ").map(_.toInt)
    opp = Opp(Array(inv0Opp, inv1Opp, inv2Opp, inv3Opp), scoreOpp)
    System.err.println(s"\t$inv0Opp $inv1Opp $inv2Opp $inv3Opp $scoreOpp")
  }

  def canCast(casts: List[Spell], inv: Array[Int]) = {
    val total = inv.sum
    casts.filter(cast => {
      cast.castable &&
        cast.delta.sum + total <= 10 &&
        cast.delta.zipWithIndex.forall(d => {
          inv(d._2) + d._1 >= 0
        })
    })
  }

  def findBrews(spellsTypeMap: Map[SpellType, List[Spell]], inv: Array[Int]) = {
    val brews = spellsTypeMap(SpellType.BREW)
    val readyBrews = brews.filter(brew => {
      brew.delta.zipWithIndex.forall(d => {
        inv(d._2) + d._1 >= 0
      })
    })
    readyBrews
  }

  def findCasts(spellsTypeMap: Map[SpellType, List[Spell]], inv: Array[Int]) = {
    val casts = spellsTypeMap(SpellType.BREW)
    val readyCasts = canCast(casts, inv)
    readyCasts
  }

  def findLearn(spellsTypeMap: Map[SpellType, List[Spell]], inv: Array[Int]) = {
    spellsTypeMap.getOrElse(SpellType.LEARN, List.empty).filter(_.tomeIndex <= inv(0))
  }

  def growTree(commands: List[Node]) = {
    
  }

  def tree(commands: List[Spell]): List[Node] = {
    val roots = commands.map(command => Node(command, command, me.inv, null, List.empty[Node]))
    growTree(roots)
    roots
  }

  def findAction = {
    val spellsTypeMap = allSpells.values.toList.groupBy(_.spellType)
    val brews = findBrews(spellsTypeMap, me.inv)
    val casts = findCasts(spellsTypeMap, me.inv)
    val learns = findLearn(spellsTypeMap, me.inv)

    val readySpells = brews ::: casts ::: learns ::: Nil
    tree(readySpells)
  }

  // game loop
  while (true) {
    readData()
    //    brewRoadmap
    val action = findAction
    // in the first league: BREW <id> | WAIT; later: BREW <id> | CAST <id> [<times>] | LEARN <id> | REST | WAIT
    println(s"$action")
  }
}