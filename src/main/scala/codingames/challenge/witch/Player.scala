package codingames.challenge.witch

import Player.SpellType.SpellType

import math._
import scala.collection.mutable
import scala.io.Source
import scala.util._
import scala.io.StdIn._

object Player extends App {

  //------------------------------------------FILE ENTRY------------------------------------------------------------------
  val filename = "resources/witch/witch1.txt"
  val bufferedSource = Source.fromFile(filename)
  val data = bufferedSource.getLines
  def readInt = if (data.hasNext) data.next.toInt else { System.exit(0); -1 }
  def readLine = if (data.hasNext) data.next else { System.exit(0); "" }
  //----------------------------------------------------------------------------------------------------------------------

  object SpellType extends Enumeration {
    type SpellType = Value
    val BREW, CAST, OPPCAST, LEARN, REST = Value
  }

  type Delta = Array[Int]
  type SpellFilter = Iterable[Spell] => Iterable[Spell]
  type STM = Map[SpellType, Iterable[Spell]]

  import SpellType._

  var next: Option[String] = None
  val actionTypeMap = Map("BREW" -> SpellType.BREW, "CAST" -> SpellType.CAST, "OPPONENT_CAST" -> SpellType.OPPCAST, "LEARN" -> SpellType.LEARN)

  sealed abstract class Pers(val inv: Delta, val score: Int) {}

  case class Result(id: Int, brewId: Int, newBrew: Delta, newAppliedDeltaSum: Int, oldAppliedDeltaSum: Int, initDeltaSum: Int, profit: Int) {
    override def toString: String = s"$id\tnewDelta=[${newBrew.mkString(",")}]\tnewSum=$newAppliedDeltaSum\toldSum=$oldAppliedDeltaSum\tprofit=$profit"
  }

  case class Me(override val inv: Delta, override val score: Int) extends Pers(inv, score)

  case class Opp(override val inv: Delta, override val score: Int) extends Pers(inv, score)

  sealed abstract class Spell(val id: Int, val spellType: SpellType, val delta: Delta, val price: Int, val tomeIndex: Int, val taxCount: Int, val castable: Boolean, val repeatable: Boolean) {
    def getCommand = "REST"

    def getNewInv(inv: Delta): Delta = delta.zipWithIndex.map(d => d._1 + inv(d._2))

    def getNewDelta(inv: Delta): Delta = delta.zipWithIndex.map(d => d._1 + inv(d._2))

    def apply(brew: Brew, oldInv: Delta): Result

    def +(spell: Spell) = delta.zip(spell.delta).map { case (x, y) => -x - y }

    override def toString: String = s"$id newDelta=[${delta.mkString(",")}] price=$price tome=$tomeIndex tax=$taxCount cast=$castable rep=$repeatable"
  }

  case class Rest(override val id: Int, override val spellType: SpellType, override val delta: Delta, override val price: Int, override val tomeIndex: Int, override val taxCount: Int, override val castable: Boolean, override val repeatable: Boolean) extends Spell(-1, REST, null, -1, -1, -1, false, false) {
    def apply(brew: Brew, oldInv: Delta): Result = Result(id, -1, Array(-1, -1, -1, -1), -4, -4, -4, 0)
  }

  case class Brew(override val id: Int, override val spellType: SpellType, override val delta: Delta, override val price: Int, override val tomeIndex: Int, override val taxCount: Int, override val castable: Boolean, override val repeatable: Boolean) extends Spell(id, spellType, delta, price, tomeIndex, taxCount, castable, repeatable) {
    lazy val deltaSum: Int = delta.sum

    override def getCommand = s"BREW $id"

    override def getNewDelta(inv: Delta): Delta = delta.zipWithIndex.withFilter(_._1 < 0).map(d => d._1 + inv(d._2))

    override def apply(affectedBrew: Brew, oldInv: Delta): Result = {
      val newInv = getNewInv(oldInv)
      val oldDeltaSum = affectedBrew.getNewDelta(oldInv).sum
      val newDeltaSum = affectedBrew.getNewDelta(newInv).sum
      Result(id, affectedBrew.id, affectedBrew.delta, newDeltaSum, oldDeltaSum, affectedBrew.delta.sum, 0)
    }
  }

  case class Cast(override val id: Int, override val spellType: SpellType, override val delta: Delta, override val price: Int, override val tomeIndex: Int, override val taxCount: Int, override val castable: Boolean, override val repeatable: Boolean) extends Spell(id, spellType, delta, price, tomeIndex, taxCount, castable, repeatable) {
    override def getCommand: String = if (castable) s"CAST $id" else super.getCommand

    override def getNewDelta(inv: Delta): Delta = delta.zipWithIndex.map(d => d._1 + inv(d._2))

    override def apply(affectedBrew: Brew, oldInv: Delta): Result = {
      val newInv = getNewInv(oldInv)
      val oldDelta = affectedBrew.getNewDelta(oldInv)
      val newDelta = affectedBrew.getNewDelta(newInv)
      val oldDeltaSum = oldDelta.sum
      val newDeltaSum = newDelta.sum
      Result(id, affectedBrew.id, newDelta, newDeltaSum, oldDeltaSum, affectedBrew.deltaSum, oldDeltaSum - newDeltaSum)
    }
  }

  case class OppCast(override val id: Int, override val spellType: SpellType, override val delta: Delta, override val price: Int, override val tomeIndex: Int, override val taxCount: Int, override val castable: Boolean, override val repeatable: Boolean) extends Spell(id, spellType, delta, price, tomeIndex, taxCount, castable, repeatable) {
    override def getNewDelta(inv: Delta): Delta = delta.zipWithIndex.withFilter(_._1 < 0).map(d => d._1 + inv(d._2))

    override def apply(affectedBrew: Brew, oldInv: Delta): Result = {
      val newInv = getNewInv(delta)
      val oldDelta = affectedBrew.getNewDelta(oldInv)
      val newDelta = affectedBrew.getNewDelta(newInv)
      val oldDeltaSum = oldDelta.sum
      val newDeltaSum = newDelta.sum
      Result(id, affectedBrew.id, newDelta, newDeltaSum, oldDeltaSum, affectedBrew.deltaSum, oldDeltaSum - newDeltaSum)
    }
  }

  case class Learn(override val id: Int, override val spellType: SpellType, override val delta: Delta, override val price: Int, override val tomeIndex: Int, override val taxCount: Int, override val castable: Boolean, override val repeatable: Boolean) extends Spell(id, spellType, delta, price, tomeIndex, taxCount, castable, repeatable) {
    override def getNewInv(inv: Delta): Delta = Array(inv(0) - tomeIndex, inv(1), inv(2), inv(3))

    override def getCommand = s"LEARN $id"

    override def getNewDelta(inv: Delta): Delta = delta.zipWithIndex.map(d => d._1 + inv(d._2))

    override def apply(affectedBrew: Brew, oldInv: Delta): Result = {
      val oldCastInv = getNewInv(oldInv)
      val newCastInv = super.getNewInv(oldCastInv)
      val oldDelta = affectedBrew.getNewDelta(oldCastInv)
      val newDelta = affectedBrew.getNewDelta(newCastInv)
      val oldDeltaSum = oldDelta.sum
      val newDeltaSum = newDelta.sum
      Result(id, affectedBrew.id, newDelta, newDeltaSum, oldDeltaSum, affectedBrew.deltaSum, oldDeltaSum - newDeltaSum)
    }
  }

  case class BrewNode(brew: Brew, spellAmount: Delta, parent: BrewNode, branch: mutable.Map[Cast, BrewNode]) {
    def apply(inv: Delta): BrewNode = BrewNode(brew, brew.getNewDelta(inv), this, mutable.Map.empty)

    def sum: Int = spellAmount.filter(_ < 0).sum

    override def toString: String = s"${brew.id}, [${spellAmount.mkString(",")}], $sum, parent is${if (parent == null) "" else " NOT"} null, branchSize=${branch.size}"
  }

  var exhaustedFound = false
  var allSpells: Map[Int, Spell] = Map.empty
  var me: Me = _
  var opp: Opp = _

  def createSpell(id: Int, spellType: SpellType, delta: Delta, price: Int, tomeIndex: Int, taxCount: Int, castable: Boolean, repeatable: Boolean): Spell = {
    spellType match {
      case SpellType.BREW => Brew(id, spellType, delta, price, tomeIndex, taxCount, castable, repeatable)
      case SpellType.CAST => Cast(id, spellType, delta, price, tomeIndex, taxCount, castable, repeatable)
      case SpellType.OPPCAST => OppCast(id, spellType, delta, price, tomeIndex, taxCount, castable, repeatable)
      case SpellType.LEARN => Learn(id, spellType, delta, price, tomeIndex, taxCount, castable, repeatable)
      case SpellType.REST => Rest(-1, REST, null, -1, -1, -1, false, false)
      case _ => throw new IllegalArgumentException("Unknown Spell type")
    }
  }

  def readData(): Unit = {
    val actionCount = readLine.toInt // the number of spells and recipes in play
    //    Console.err.println(s"$actionCount")
    allSpells = Map.empty

    // System.err.println(s"ID\tType\td0\td1\td2\td3\tPrice\tTome\tTax\tCast\tRep")
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
      //      Console.err.println(s"${_actionId}\t$actionType\t${_delta0}\t${_delta1}\t${_delta2}\t${_delta3}\t${_price}\t${_tomeIndex}\t${_taxCount}\t${_castable}\t${_repeatable}")
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
    allSpells = allSpells + (-1 -> Rest(-1, REST, null, -1, -1, -1, false, false))
    // inv0: tier-0 ingredients in inventory
    // score: amount of rupees
    val Array(inv0Me, inv1Me, inv2Me, inv3Me, scoreMe) = (readLine split " ").map(_.toInt)
    me = Me(Array(inv0Me, inv1Me, inv2Me, inv3Me), scoreMe)
    //    Console.err.println(s"\t$inv0Me $inv1Me $inv2Me $inv3Me $scoreMe")
    val Array(inv0Opp, inv1Opp, inv2Opp, inv3Opp, scoreOpp) = (readLine split " ").map(_.toInt)
    opp = Opp(Array(inv0Opp, inv1Opp, inv2Opp, inv3Opp), scoreOpp)
    //    Console.err.println(s"\t$inv0Opp $inv1Opp $inv2Opp $inv3Opp $scoreOpp")
  }

  def canButExhaustedCast(casts: Iterable[Spell]) = {
    val total = me.inv.sum
    casts.filter(cast => {
      !cast.castable &&
        cast.delta.sum + total <= 10 &&
        cast.delta.zipWithIndex.forall(d => {
          me.inv(d._2) + d._1 >= 0
        })
    })
  }

  def canAndExhaustedCast(casts: Iterable[Spell]) = {
    val total = me.inv.sum
    casts.filter(cast => {
      cast.delta.sum + total <= 10 &&
        cast.delta.zipWithIndex.forall(d => {
          me.inv(d._2) + d._1 >= 0
        })
    })
  }

  def canCast(cast: Spell, total: Int, inv: Delta): Boolean = {
    cast.castable &&
      cast.delta.sum + total <= 10 &&
      cast.delta.zipWithIndex.forall(d => {
        inv(d._2) + d._1 >= 0
      })
  }

  def canCast(casts: Iterable[Spell], inv: Delta): Iterable[Spell] = {
    val total = inv.sum
    casts.filter(cast => canCast(cast, total, inv))
  }

  def canCast(casts: Iterable[Spell]): Iterable[Spell] = canCast(casts, me.inv)

  def canLearn(learns: Iterable[Spell]): Iterable[Spell] = learns.filter(_.tomeIndex <= me.inv(0))

  def compareBrewNodes(castData: (Cast, BrewNode)) = castData._2.parent.sum < castData._2.sum

  def enrichBrewNodes(brewNodes: Iterable[BrewNode], casts: Iterable[Spell]) {
    brewNodes.foreach(bn => {
      casts.map(cast => {
        val castCalcRes = cast.getNewDelta(me.inv)
        val leaf = bn.apply(castCalcRes)
        bn.branch.put(cast.asInstanceOf[Cast], leaf)
      })
    })
  }

  def canBrew(brew: Spell, inv: Delta): Boolean = brew.delta.zipWithIndex.forall(d => {
    inv(d._2) + d._1 >= 0
  })

  def canBrew(brews: Iterable[Spell]): Iterable[Spell] = brews.filter(brew => canBrew(brew, me.inv)).toList

  def findBrew(spellsTypeMap: Map[SpellType, Iterable[Spell]]): Option[Int] = {
    val brews = spellsTypeMap(SpellType.BREW)
    val readyBrews = canBrew(brews)
    if (readyBrews.nonEmpty) {
      val bestBrew = readyBrews.maxBy(_.price)
      Some(bestBrew.id)
    } else None
  }

  def findExhaustedCast(spellsTypeMap: Map[SpellType, Iterable[Spell]]): Option[(Int, Int)] = {
    val casts = spellsTypeMap(SpellType.CAST)
    val brews = spellsTypeMap(SpellType.BREW)
    val readyCasts = canButExhaustedCast(casts)
    val brewNodes = brews.map(brew => BrewNode(brew.asInstanceOf[Brew], brew.getNewDelta(me.inv), null, mutable.Map.empty))
    enrichBrewNodes(brewNodes, readyCasts)

    val res = brewNodes.flatMap(_.branch).filter(compareBrewNodes)
    if (res.nonEmpty) {
      val brewNodeData = res.maxBy(compareBrewNodes)
      // Console.err.println(s"prevSum=${brewNodeData._2.parent.sum}\tcurrentSum=${brewNodeData._2.sum}\tbrew=${brewNodeData._2.brew.id}")
      exhaustedFound = true
      Some(brewNodeData._1.id, brewNodeData._2.parent.sum - brewNodeData._2.sum)
    } else None
  }

  def findCast(spellsTypeMap: Map[SpellType, Iterable[Spell]], spellFilter: SpellFilter): Option[(Int, Int)] = {
    val casts = spellsTypeMap(SpellType.CAST)
    val brews = spellsTypeMap(SpellType.BREW)
    val readyCasts = spellFilter(casts)
    val brewNodes = brews.map(brew => BrewNode(brew.asInstanceOf[Brew], brew.getNewDelta(me.inv), null, mutable.Map.empty))
    enrichBrewNodes(brewNodes, readyCasts)

    val res = brewNodes.flatMap(_.branch).filter(compareBrewNodes)
    if (res.nonEmpty) {
      val brewNodeData = res.maxBy(compareBrewNodes)
      // Console.err.println(s"prevSum=${brewNodeData._2.parent.sum}\tcurrentSum=${brewNodeData._2.sum}\tbrew=${brewNodeData._2.brew.id}")
      Some(brewNodeData._1.id, brewNodeData._2.parent.sum - brewNodeData._2.sum)
    } else None
  }

  def findLearn(spellsTypeMap: Map[SpellType, Iterable[Spell]]): Option[(Int, Int)] = {
    //    val casts = spellsTypeMap(SpellType.CAST)
    val brews = spellsTypeMap(SpellType.BREW)
    val readyLearns = spellsTypeMap.getOrElse(SpellType.LEARN, Iterable.empty).filter(_.tomeIndex <= me.inv(0))

    val r = readyLearns.map(learn => {
      Cast(learn.id, SpellType.CAST, learn.delta, learn.price, -1, -1, castable = true, repeatable = learn.repeatable) /*, learn.newInv(me.inv))*/
    })
    /*.filter(data => canCast(data._1, data._2. sum, data._2))*/
    val brewNodes = brews.map(brew => BrewNode(brew.asInstanceOf[Brew], brew.getNewDelta(me.inv), null, mutable.Map.empty))
    enrichBrewNodes(brewNodes, r)
    val res = brewNodes.flatMap(_.branch).filter(compareBrewNodes)
    if (res.nonEmpty) {
      val brewNodeData = res.maxBy(compareBrewNodes)
      Some((brewNodeData._1.id, brewNodeData._2.parent.sum - brewNodeData._2.sum))
    } else None
    /*
    if (readyLearns.nonEmpty) {
      Some(readyLearns.maxBy(_.price).id)
    } else None*/
  }

  def estimateInv(inv: Delta, koeff: Delta) = inv.zipWithIndex.map(ingr => ingr._1 * koeff(ingr._2)).sum
  def estimateInvs(castInvs: Iterable[(Spell, Delta)], koeff: Delta) = castInvs.map(castInv => (castInv._1, estimateInv(castInv._2, koeff)))

  def brewIsGoing(spellsTypeMap: STM): Option[Int] = {
    val brews = spellsTypeMap(BREW).map(_.asInstanceOf[Brew]).toList.sortBy(-_.price)
    val readyBrews = canBrew(brews)
    if (readyBrews.nonEmpty) Some(readyBrews.maxBy(_.price).id)
    else {
      //      val casts = canAndExhaustedCast(spellsTypeMap(CAST))
      val casts = canCast(spellsTypeMap(CAST))
      //      val casts = spellsTypeMap(CAST)
      val canCastBrews = casts.flatMap(cast => {
        brews.withFilter(brew => {
          val newInv = cast.getNewInv(me.inv)
          canBrew(brew, newInv)
        }).map(brew => (brew, cast))
      })
      if (canCastBrews.nonEmpty) Some(canCastBrews.maxBy(_._1.price)._2.id) else None
    }
  }

  def learnIsGoing(spellsTypeMap: STM): Option[Int] = {
    spellsTypeMap.get(LEARN) match {
      case Some(learns) =>
        val koeff = spellsTypeMap(BREW).map(_.delta).reduce((d1, d2) => d1.zip(d2).map { case (n1, n2) => n1 + n2 }).map(-_)
        val readyLearn = canLearn(learns)
        val newInvs = readyLearn.map(learn => (learn, learn.getNewInv(me.inv)))
        val oldInvEstimation = estimateInv(me.inv, koeff)
        val invsEstimation = estimateInvs(newInvs, koeff)
        val bestInv = invsEstimation.maxBy(_._2)
        val bestInvEstimation = bestInv._2
        if (bestInvEstimation >= oldInvEstimation) Some(bestInv._1.id) else None
      case None => None
    }
  }

  def castIsGoing(spellsTypeMap: STM): Option[Int] = {
    val casts = spellsTypeMap(CAST)
    val readyCasts = canCast(casts)
    val koeff = spellsTypeMap(BREW).map(brew => brew.delta.map(_ * brew.price)).reduce((d1, d2) => d1.zip(d2).map { case (n1, n2) => n1 + n2 }).map(-_)
    //    var koeff = Array(0,0,0,0)
    //    val rrr = for (brew <- spellsTypeMap(BREW)) yield {
    //       koeff.zip(brew.delta).map { case (n1, n2) => n1 + n2 }
    //    }
    //    val koeff = spellsTypeMap(BREW).map(_.delta) {
    //      case d => ((d1, d2) => d1.zip(d2).map { case (n1, n2) => n1 + n2 }).map(-_)
    //    }
    val exhaustedCasts = canButExhaustedCast(casts)
    val castList = if (exhaustedCasts.size > readyCasts.size) readyCasts ++ exhaustedCasts else readyCasts
    //    val castList = if (readyCasts.isEmpty) exhaustedCasts else readyCasts
    val newInvs = castList.map(cast => (cast, cast.getNewInv(me.inv)))
    val oldInvEstimation = estimateInv(me.inv, koeff)
    val invsEstimation = estimateInvs(newInvs, koeff)
    val bestInv = invsEstimation.maxBy(_._2)
    val bestInvEstimation = bestInv._2
    if (bestInvEstimation >= oldInvEstimation) Some(bestInv._1.id) else None
  }

  def calculateAction: Int = {
    val spellsTypeMap = allSpells.values.groupBy(_.spellType)
    brewIsGoing(spellsTypeMap).getOrElse(castIsGoing(spellsTypeMap).getOrElse(learnIsGoing(spellsTypeMap).getOrElse(-1)))
  }

  def findAction: String = {
    allSpells(calculateAction).getCommand
  }

  // game loop
  while (true) {
    readData()
    val action = findAction
    // in the first league: BREW <id> | WAIT; later: BREW <id> | CAST <id> [<times>] | LEARN <id> | REST | WAIT
    println(s"$action")
  }
}