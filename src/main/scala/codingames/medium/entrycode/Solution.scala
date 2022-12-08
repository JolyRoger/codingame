package codingames.medium.entrycode

import scala.collection.mutable
import scala.io.Source
import scala.io.StdIn.readLine

class Accumulator[T] {
  var listAccu: List[List[T]] = Nil
  var mapAccu: mutable.Map[List[T], List[Int]] = new mutable.HashMap[List[T], List[Int]]()
  var counter = 0
  def clear(): Unit = {
    listAccu = Nil
    mapAccu.clear()
    counter = 0
  }
  def push(listElem: List[T]): Unit = {
    listAccu = listElem :: listAccu
    val mapElem = mapAccu.getOrElse(listElem, Nil)
    mapAccu.put(listElem, counter :: mapElem)
    counter += 1
  }
  def getSize() = counter
  def getSizeDistinct() = mapAccu.size
  def getSize(listElem: List[T]) = mapAccu.getOrElse(listElem, Nil).length
  def getIndexFirst(listElem: List[T]) = mapAccu.getOrElse(listElem, List(-1)).head
}

object Solution extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
   val filename = "resources/entrycode/1.txt"
   val bufferedSource = Source.fromFile(filename)
   val data = bufferedSource.getLines
   def readInt = if (data.hasNext) data.next.toInt else { System.exit(0); -1 }
   def readLine = if (data.hasNext) data.next else { System.exit(0); "" }
//----------------------------------------------------------------------------------------------------------------------
  type ValMap = mutable.Map[String, mutable.Set[String]]
  val digNum = readLine.toInt
  val entryLength = readLine.toInt
  Console.err.println(s"$digNum $entryLength")

  val range = 0 until digNum

  def repeatingPerm[T](elems: List[T], genSize: Int, f: List[T] => Unit): Unit = {
    def repeatingPermRec(elems: List[T], depth: Int, partResult: List[T]): Unit = depth match {
        case 0 => f(List())
        case 1 => for (elem <- elems) f(elem :: partResult)
        case n => for (elem <- elems) repeatingPermRec(elems, n - 1, elem :: partResult)
    }
    if (genSize < 0) throw new IllegalArgumentException("Negative lengths not allowed in repeatingPerm...")
    repeatingPermRec(elems, genSize, Nil)
  }

  val accumulator = new Accumulator[Int]
  repeatingPerm(range.toList, entryLength, accumulator.push)
  val combinations = mutable.Set() ++ accumulator.listAccu.map(_.mkString).toSet

  var sequence = (for (_ <- 0 until entryLength) yield "0").mkString.init

  def mapKey(sequence: String, entryLength: Int) = sequence.substring(sequence.length - entryLength + 1, sequence.length)

  def removeVal(toRemove: String, dataMap: ValMap) = {
    val mk = mapKey(sequence, entryLength)
    val vals = dataMap(mk)
    vals.remove(toRemove)
    dataMap.put(mk, vals)
    toRemove
  }

//  Console.err.println(s"$sequence")
//  Console.err.println(s"$combinations")

  def notEmpty(seqMap: ValMap) = seqMap.values.exists(_.nonEmpty)

  var index = 1
  var removed = ""
  do {
    val mm = mutable.Map() ++ combinations.groupBy(str => str.init)
    combinations.clear
//    Console.err.println(s"$mm")

    while (notEmpty(mm)) {
      val read = mapKey(sequence, entryLength)
      val unreadValues = mm(read)
      if (unreadValues.nonEmpty) {
        val minValue = unreadValues.min
        val lastSym = minValue.last
        removed = removeVal(minValue, mm)
        sequence = sequence + lastSym
      } else {
        sequence = sequence.init
        combinations.add(removed)
      }
    }
  } while(combinations.nonEmpty)
//  Console.err.println(s"seq: $sequence")
  println(s"$sequence")
}