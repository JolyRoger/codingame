package codingames.challenge.pikaptcha.l1

import scala.io.StdIn._

object Player extends App {
  val Array(width, height) = for (i <- readLine split " ") yield i.toInt
  val arr = (for (i <- 0 until height) yield readLine).toArray.map(_.toCharArray)

  var intArr = Array.fill[Int](width)(-1)
  var intDoubleArr = Array.fill[Array[Int]](height)(intArr.clone)

  for (i <- arr.indices; j <- arr(i).indices) {
    if (arr(i)(j) == '0') {
      intDoubleArr(i)(j) = 0

      if (i - 1 >= 0 && arr(i - 1)(j) == '0') {
        intDoubleArr(i)(j) += 1
      }
      if (i + 1 < height && arr(i + 1)(j) == '0') {
        intDoubleArr(i)(j) += 1
      }
      if (j - 1 >= 0 && arr(i)(j - 1) == '0') {
        intDoubleArr(i)(j) += 1
      }
      if (j + 1 < width && arr(i)(j + 1) == '0') {
        intDoubleArr(i)(j) += 1
      }
    }
  }
  intDoubleArr.map(arr => arr.mkString("", "", "").replace("-1", "#")).foreach(println)
}