package codingames.medium.entrycode

import scala.collection.mutable
import scala.io.Source
import scala.io.StdIn.readLine

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
  var removed = ""
  val range = 0 until digNum
  var targetList = List.empty[List[Int]]
  lazy val combinations = mutable.Set() ++ targetList.map(_.mkString).toSet
  var sequence = (for (_ <- 0 until entryLength) yield "0").mkString.init

  Console.err.println(s"$digNum $entryLength")

  def notEmpty(seqMap: ValMap) = seqMap.values.exists(_.nonEmpty)

  def push(entryList: List[Int]) {
    targetList ::= entryList
  }

  def repeatingPermutations(elements: List[Int], depth: Int, partResult: List[Int], f: List[Int] => Unit): Unit = depth match {
      case 0 => f(Nil)
      case 1 => for (element <- elements) f(element :: partResult)
      case n => for (element <- elements) repeatingPermutations(elements, n - 1, element :: partResult, f)
  }

  def mapKey(sequence: String, entryLength: Int) = sequence.substring(sequence.length - entryLength + 1, sequence.length)

  def removeVal(toRemove: String, dataMap: ValMap) = {
    val mk = mapKey(sequence, entryLength)
    val vals = dataMap(mk)
    vals.remove(toRemove)
    dataMap.put(mk, vals)
    toRemove
  }

  repeatingPermutations(range.toList, entryLength, Nil, push)

  do {
    val dataMap = mutable.Map() ++ combinations.groupBy(_.init)
    combinations.clear
    while (notEmpty(dataMap)) {
      val unreadValues = dataMap(mapKey(sequence, entryLength))
      sequence = if (unreadValues.nonEmpty) {
        removed = removeVal(unreadValues.min, dataMap)
        sequence + removed.last
      } else {
        combinations.add(removed)
        sequence.init
      }
    }
  } while(combinations.nonEmpty)

  println(s"$sequence")
}