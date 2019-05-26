package codingames.challenge.iceandfire.src

import scala.collection.mutable

case class Board(boardMatrix: Array[Array[Char]]) {

  val w = 12
  val N = w * w

  def toMatrix(number: Int): (Int, Int) = (number % w, number / w)
  implicit def toNumber(point: (Int, Int)): Int = point._2 * w + point._1 % w

  def allClosest(x: Int, y: Int) = List((x-1, y), (x, y-1), (x+1, y), (x, y+1)).filter(
    square => square._1 >= 0 && square._2 >= 0 && square._1 < 12 && square._2 < 12 &&
      boardMatrix(square._2)(square._1) != '#').map(toNumber)

  val adj = new Array[List[Int]](w * w)

  for (row <- 0 until 12; col <- 0 until 12; if boardMatrix(row)(col) != '#')
    adj(toNumber((col, row))) = allClosest(col, row)

  val topLeftBfs = bfs(0)
  val bottomDownBfs = bfs(w * w - 1)

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