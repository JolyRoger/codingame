package codingames.challenge.witch

import codingames.challenge.witch.Player.{Spell, me}

import math._
import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util._
import scala.io.StdIn._

object Player2 extends App {

  //------------------------------------------FILE ENTRY------------------------------------------------------------------
                                                            val filename = "resources/witch/witch2.txt"
                                                            val bufferedSource = Source.fromFile(filename)
                                                            val data = bufferedSource.getLines
                                                            def readInt = if (data.hasNext) data.next.toInt else -1
                                                            def readLine = if (data.hasNext) data.next else "EOF"
  //----------------------------------------------------------------------------------------------------------------------

  object SpellType extends Enumeration {
    type SpellType = Value
    val BREW, CAST, OPPCAST, LEARN, REST = Value
  }
  import SpellType._

  type SpellFilter = List[Spell] => List[Spell]
  type Inventory = Array[Int]
  type SpellMap = Map[Int, Spell]
  type SpellTypeMap = Map[SpellType, List[Spell]]

  var next: Option[String] = None
  val actionTypeMap = Map("BREW" -> SpellType.BREW, "CAST" -> SpellType.CAST, "OPPONENT_CAST" -> SpellType.OPPCAST,  "LEARN" -> SpellType.LEARN)
  var allSpells: SpellMap = Map.empty

  case class EstimationData(level: Int, negativeSum: Int, initSum: Int, rootSpellId: Int, rootPrice: Int) {
    override def toString: String = s"level=$level res=$negativeSum init=${initSum} root=${allSpells.get(rootSpellId).map(_.getCommand)} rootPrice=${rootPrice}"
  }
  sealed abstract class Pers(val inv: Inventory, val score: Int)
  case class Me(override val inv: Inventory, override val score: Int) extends Pers(inv, score)
  case class Opp(override val inv: Inventory, override val score: Int) extends Pers(inv, score)
  sealed abstract class Spell(val id: Int, val spellType: SpellType, val delta: Inventory, val price: Int, val tomeIndex: Int, val taxCount: Int, val castable: Boolean, val repeatable: Boolean) {
    def affect(spells: SpellMap, inv: Inventory): SpellMap = spells
    def getCommand = s"${spellType.toString} $id"
    def applyInv(inv: Inventory): Inventory = delta.zipWithIndex.map(d => d._1 + inv(d._2)) // applies spell to inv and returns inv
    def newDelta(inv: Inventory): Inventory = delta                                         // applies inv to delta and returns delta
    def currentSum: Int = delta.sum
    def totalSum(inv: Inventory): Int = applyInv(inv).sum
    override def toString: String = getCommand
  }
  case class Rest(override val id: Int, override val spellType: SpellType, override val delta: Inventory, override val price: Int, override val tomeIndex: Int, override val taxCount: Int, override val castable: Boolean, override val repeatable: Boolean) extends Spell(id, spellType, delta, price, tomeIndex, taxCount, castable, repeatable) {
    override def affect(spells: SpellMap, inv: Inventory): SpellMap = spells.map(castEntry => {
      val cast = castEntry._2
      if (cast.spellType == CAST) id -> Cast(cast.id, cast.spellType, cast.delta, cast.price, cast.tomeIndex, cast.taxCount, castable = true, cast.repeatable)
      else castEntry
    })
    override def applyInv(inv: Inventory): Inventory = inv
    override def toString: String = "REST"
  }
  case class Brew(override val id: Int, override val spellType: SpellType, override val delta: Inventory, override val price: Int, override val tomeIndex: Int, override val taxCount: Int, override val castable: Boolean, override val repeatable: Boolean) extends Spell(id, spellType, delta, price, tomeIndex, taxCount, castable, repeatable) {
    override def affect(spells: SpellMap, inv: Inventory): SpellMap = spells - id
    override def newDelta(inv: Inventory): Inventory = delta.zipWithIndex.map(d => {
      val res = d._1 + inv(d._2)
      if (res > 0) 0 else res
    })
    override def currentSum: Int = delta.filter(_ < 0).sum
    override def totalSum(inv: Inventory): Int = applyInv(inv).filter(_ < 0).sum
  }
  case class Cast(override val id: Int, override val spellType: SpellType, override val delta: Inventory, override val price: Int, override val tomeIndex: Int, override val taxCount: Int, override val castable: Boolean, override val repeatable: Boolean) extends Spell(id, spellType, delta, price, tomeIndex, taxCount, castable, repeatable) {
    override def affect(spells: SpellMap, inv: Inventory): SpellMap = {
      val newSpells = spells - id
      newSpells + (id -> Cast(id, spellType, delta, price, tomeIndex, taxCount, castable = false, repeatable))
    }
  }
  case class Learn(override val id: Int, override val spellType: SpellType, override val delta: Inventory, override val price: Int, override val tomeIndex: Int, override val taxCount: Int, override val castable: Boolean, override val repeatable: Boolean) extends Spell(id, spellType, delta, price, tomeIndex, taxCount, castable, repeatable) {
    override def applyInv(inv: Inventory): Inventory = inv
    override def affect(spells: SpellMap, inv: Inventory): SpellMap = {
      val newSpells = spells - id
      newSpells + (id -> Cast(id, SpellType.CAST, delta, price, -1, -1, castable = true, repeatable))
    }
  }
  case class OppCast(override val id: Int, override val spellType: SpellType, override val delta: Inventory, override val price: Int, override val tomeIndex: Int, override val taxCount: Int, override val castable: Boolean, override val repeatable: Boolean) extends Spell(id, spellType, delta, price, tomeIndex, taxCount, castable, repeatable) {}
  case class Node(root: Spell, spell: Spell, newInv: Inventory, parent: Node, branch: List[Node]) {
    override def toString: String = s"${root.getCommand}, newInv=[${newInv.mkString(",")}], parent is${if (parent == null) "" else " NOT"} null, branchSize=${branch.size}"
  }

  var estimation: Map[Int, EstimationData] = Map.empty
  var me: Me = _
  var opp: Opp = _

  def findBrews(spellsTypeMap: SpellMap, inv: Inventory) = {
    val readyBrews = spellsTypeMap.filter(brew => {
      brew._2.spellType == SpellType.BREW &&
      brew._2.delta.zipWithIndex.forall(d => {
        inv(d._2) + d._1 >= 0
      })
    })
    readyBrews
  }

  def findCasts(spellsMap: SpellMap, inv: Inventory) = {
    val total = inv.sum
    spellsMap.filter(cast => {
      cast._2.spellType == SpellType.CAST &&
        cast._2.castable &&
        cast._2.delta.sum + total <= 10 &&
        cast._2.delta.zipWithIndex.forall(d => {
          inv(d._2) + d._1 >= 0
        })
    })
  }

  def findLearn(spellsMap: SpellMap, inv: Inventory) = {
    spellsMap.filter(learn => learn._2.spellType == SpellType.LEARN && learn._2.tomeIndex <= inv(0))
  }

  def findReadySpells(allSpells: SpellMap, inv: Inventory) = {
    val brews = findBrews(allSpells, inv)
    val casts = findCasts(allSpells, inv)
    val learns = findLearn(allSpells, inv)
    brews ++ casts ++ learns ++ Map(-1 -> Rest(-1, REST, Array(0,0,0,0), 0, 0, 0, castable = false, repeatable = false))
  }

  case class Node2(level: Int, root: Int, /*from: Spell, */readySpells: Map[Int, Spell], spellMap: SpellMap, inv: Inventory) {
    private lazy val brews = spellMap.values.groupBy(_.spellType)(BREW)
    private def updateEstimation = {
      brews.foreach(brew => {
        val csum = brew.newDelta(inv).sum
        estimation = estimation.get(brew.id) match {
          case Some(data) =>
            if (data.negativeSum < csum) estimation + (brew.id -> EstimationData(level, csum, data.initSum, root, brew.price))
            else estimation
          case None => estimation + (brew.id -> EstimationData(level + 1, csum, brew.currentSum, root, brew.price))
        }
      })
      this
    }
    private def apply(spellEntry: (Int, Spell)): Node2 = {
        val spell = spellEntry._2
        val newInv = spell.applyInv(inv)
        val newSpells = spell.affect(spellMap, newInv)
        val rootCommandId = if (root == Int.MinValue) spell.id else root
//        updateEstimation(rootCommandId, newInv)
        Node2(level + 1, rootCommandId, findReadySpells(newSpells, newInv), newSpells, newInv).updateEstimation
    }
    def apply: List[Node2] = readySpells.map(apply).toList
    override def toString: String = s"$root: [${inv.mkString(",")}]"
  }

  def createSpell(id: Int, spellType: SpellType, delta: Inventory, price: Int, tomeIndex: Int, taxCount: Int, castable: Boolean, repeatable: Boolean): Spell = {
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
    Console.err.println(s"$actionCount")
    allSpells = Map.empty

    Console.err.println(s"ID\tType\td0\td1\td2\td3\tPrc\tTom\tTax\tCst\tRep")
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
      Console.err.println(s"${_actionId}\t$actionType\t${_delta0}\t${_delta1}\t${_delta2}\t${_delta3}\t${_price}\t${_tomeIndex}\t${_taxCount}\t${_castable}\t${_repeatable}")
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
    Console.err.println(s"\t$inv0Me $inv1Me $inv2Me $inv3Me $scoreMe")
    val Array(inv0Opp, inv1Opp, inv2Opp, inv3Opp, scoreOpp) = (readLine split " ").map(_.toInt)
    opp = Opp(Array(inv0Opp, inv1Opp, inv2Opp, inv3Opp), scoreOpp)
    Console.err.println(s"\t$inv0Opp $inv1Opp $inv2Opp $inv3Opp $scoreOpp")
  }

  @tailrec
  def calculate(nodes: List[Node2], step: Int) {
    if (step == 5) estimation.foreach(est => {
      Console.err.println(s"${est._1}->${est._2}")
    }) else {
      val leaves = nodes.flatMap(node => node.apply)
      Console.err.println(s"leaves: ${leaves.size}")
      calculate(leaves, step + 1)
    }
  }


  def findAction = {
    estimation = allSpells.withFilter(_._2.spellType == BREW).map(spellEntry => (spellEntry._1, EstimationData(0, Int.MinValue, spellEntry._2.currentSum, Int.MinValue, spellEntry._2.price)))
    val commands = findReadySpells(allSpells, me.inv)
    val root = Node2(0, Int.MinValue, commands, allSpells, me.inv)
    calculate(root :: Nil, 0)

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