package codingames.challenge.xmasrush.v2

import scala.collection.mutable
import scala.math.{pow, sqrt}

class Graph(N: Int) {
  val adj = (for (i <- 0 until N) yield List[Int]()).toArray
  def addEdge(v: Int, w: Int){
    adj(v) = w :: adj(v)
    adj(w) = v :: adj(w)
  }
  def cutEdge(v: Int, w: Int): Unit = {
    adj(v) = adj(v).filter(_ != w)
    adj(w) = adj(w).filter(_ != v)
  }

  def bfs(s: Int) = {
    val marked: Array[Boolean] = new Array[Boolean](N)
    val edgeTo = Array.fill[Int](N)(Int.MaxValue)
    val distTo = Array.fill[Int](N)(Int.MaxValue)
    val q = mutable.Queue[Int]()
    var i = 0

    q.enqueue(s)
    marked(s) = true
    distTo(s) = 0
    while (q.nonEmpty) {
      val v = q.dequeue
      i = i + 1
      adj(v).filterNot(marked).foreach(
        w => {
          q.enqueue(w)
          marked(w) = true
          edgeTo(w) = v
          distTo(w) = distTo(v) + 1
        }
      )
    }
    (edgeTo, distTo)
  }
}
















object Player {
  def closest(p: (Int, Int), items: List[(Int, Int)]) = if (items.isEmpty) (-1,-1) else items.minBy(item => euclidean(p, item))
  def euclidean(a: (Int, Int), b: (Int, Int)) = sqrt(pow(b._1 - a._1, 2) + pow(b._2 - a._2, 2))
  def toMatrix(number: Int): (Int, Int) = (number / 7, number % 7)
  def toNumber(point: (Int, Int)): Int = point._1 * 7 + point._2 % 7
  def relation(p1: (Int, Int), p2: (Int, Int)) = if (p1._1 == p2._1 && p1._2 - 1 == p2._2) "DOWN" else
    if (p1._1 == p2._1 && p1._2 + 1 == p2._2) "UP" else
    if (p1._2 == p2._2 && p1._1 - 1 == p2._1) "RIGHT" else
    if (p1._2 == p2._2 && p1._1 + 1 == p2._1) "LEFT" else "NOTHING"

}
