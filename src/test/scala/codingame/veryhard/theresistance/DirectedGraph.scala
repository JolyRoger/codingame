package codingame.veryhard.theresistance

import scala.collection.mutable

class DirectedGraph(adj: Map[Int, Map[Int, Int]]) {

  def allPaths(from: Int, to: Int) = {
    val stack = mutable.Stack[(Int, Int)]()
    stack.push(from -> 1)
    var mul = 1
    var sum = 0

    while (stack.nonEmpty) {
      val (newIndex, newMul) = stack.pop
      mul = mul * newMul
      sum = adj.get(newIndex) match {
        case None =>
//          if (newIndex == to) sum + (mul * adj(fromWhere)(to)) else sum
          val nm = mul
          mul = 1
          if (newIndex == to) sum + nm else sum
        case Some(info) =>
//          val keys = info.keys
          stack.pushAll(info)
          sum
      }
    }
    sum
  }
}
