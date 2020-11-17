//package codingames.challenge.witch

import scala.annotation.tailrec
import scala.io.Source
import scala.io.StdIn._

object Player extends App {

  //------------------------------------------FILE ENTRY------------------------------------------------------------------
//                                                            val filename = "resources/witch/witch1.txt"
//                                                            val bufferedSource = Source.fromFile(filename)
//                                                            val data = bufferedSource.getLines
//                                                            def readInt = if (data.hasNext) data.next.toInt else { System.exit(0); -1 }
//                                                            def readLine = if (data.hasNext) data.next else { System.exit(0); "" }
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

  case class EstimationData(level: Int, negativeSum: Int, initSum: Int, sequence: List[Int], rootPrice: Int) {
    override def toString: String = s"level=$level res=$negativeSum init=${initSum} " +
      s"root=${sequence.map(allSpells(_).getCommand).reverse.mkString("->")} rootPrice=${rootPrice}"
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
      if (cast.spellType == CAST) cast.id -> Cast(cast.id, cast.spellType, cast.delta, cast.price, cast.tomeIndex, cast.taxCount, castable = true, cast.repeatable)
      else castEntry
    })
    override def applyInv(inv: Inventory): Inventory = inv
    override def getCommand: String = "REST"
    override def toString: String = getCommand
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
  var brewFound = false
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
    if (brews.nonEmpty) {
      brewFound = true
      brews
    } else {
      val casts = findCasts(allSpells, inv)
      val learns = findLearn(allSpells, inv)
      (brews ++ casts ++ learns) + (-1 -> allSpells(-1))
    }
  }

  case class Node2(level: Int, from: List[Int], readySpells: Map[Int, Spell], spellMap: SpellMap, inv: Inventory, proceed: Boolean) {
    private lazy val brews = allSpells.withFilter(_._2.spellType == BREW).map(brew => brew._2)
    private def proceed(spell: Spell): Boolean = !(from.nonEmpty && ((spell.spellType == REST && allSpells(from.head).spellType == REST) ||
                                                                    (spell.spellType == BREW && allSpells(from.head).spellType == BREW)))

    private def updateEstimation = {
      brews.foreach(brew => {
        val csum = brew.newDelta(inv).sum
        estimation = estimation.get(brew.id) match {
          case Some(data) =>
            if (data.negativeSum < csum) estimation + (brew.id -> EstimationData(level, csum, data.initSum, from, brew.price))
            else estimation
          case None => estimation + (brew.id -> EstimationData(level + 1, csum, brew.currentSum, from, brew.price))
        }
      })
      this
    }
    private def apply(spellEntry: (Int, Spell)): Node2 = {
        val spell = spellEntry._2
        val newInv = spell.applyInv(inv)
        val newSpells = spell.affect(spellMap, newInv)
//        if (spell.spellType == BREW && estimation(spell.id).sequence.nonEmpty && allSpells(estimation(spell.id).sequence.head).spellType != BREW)
//          estimation += (spell.id -> EstimationData(level, 0, estimation(spell.id).initSum, spell.id :: estimation(spell.id).sequence, spell.price))
        Node2(level + 1, spell.id :: from, findReadySpells(newSpells, newInv), newSpells, newInv, proceed(spell)).updateEstimation
    }
    def apply: List[Node2] = readySpells.map(apply(_)).toList
    override def toString: String = s"${from.map(allSpells(_).getCommand).reverse.mkString(" -> ")}   inv=[${inv.mkString(",")}]"
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
      allSpells += (-1 -> Rest(-1, REST, Array(0,0,0,0), 0, -1, -1, false, false))
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
    if (step == 2) estimation.foreach(est => {
    }) else {
      val leaves = nodes.flatMap(node => node.apply).filter(_.proceed)
      calculate(leaves, step + 1)

    }
  }
  def best2: String = {
    val profitCol = estimation.collect {
      case data if data._2.negativeSum + data._2.initSum < 0 => (data._2.sequence.last, data._2.negativeSum)
    }
    if (profitCol.isEmpty) {
      Console.err.println(s"PROFITCOL IS EMPTY!!!")
      "REST"
    } else allSpells(profitCol.maxBy(_._2)._1).getCommand
  }
  def best = {
    val estimationMap = estimation.values.groupBy(_.sequence.last)
    val profitData = estimationMap.map(ab => (ab._1, ab._2.map(data => data.negativeSum - data.initSum).sum))
    allSpells(profitData.maxBy(_._2)._1).getCommand
  }

  def findAction: String = {
    estimation = allSpells.withFilter(_._2.spellType == BREW).map(spellEntry => (spellEntry._1, EstimationData(0, Int.MinValue, spellEntry._2.currentSum, List.empty, spellEntry._2.price)))
    val commands = findReadySpells(allSpells, me.inv)
    if (brewFound) commands.maxBy(_._2.price)._2.getCommand else {
      val root = Node2(0, List.empty, commands, allSpells, me.inv, proceed = true)
      calculate(root :: Nil, 0)
      best2
    }
  }

  // game loop
  while (true) {
    readData()
    val action = findAction
    // in the first league: BREW <id> | WAIT; later: BREW <id> | CAST <id> [<times>] | LEARN <id> | REST | WAIT
    println(s"$action")
  }
}