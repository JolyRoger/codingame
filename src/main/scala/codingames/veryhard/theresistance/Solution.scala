//package codingames.veryhard.theresistance

import java.util.regex.{Matcher, Pattern}
import scala.collection.mutable

object Solution extends App {

  val wordMap: scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map.empty[String, Int]
  val posStartMap: scala.collection.mutable.Map[Int, Map[Int, Int]] = scala.collection.mutable.Map.empty[Int, Map[Int, Int]]

  val morze: Map[String, Char] = Map(
    ".-" ->   'A', "-..." -> 'B', "-.-." -> 'C', "-.." ->  'D',
    "." ->    'E', "..-." -> 'F', "--." ->  'G', "...." -> 'H',
    ".." ->   'I', ".---" -> 'J', "-.-" ->  'K', ".-.." -> 'L',
    "--" ->   'M', "-." ->   'N', "---" ->  'O', ".--." -> 'P',
    "--.-" -> 'Q', ".-." ->  'R', "..." ->  'S', "-" ->    'T',
    "..-" ->  'U', "...-" -> 'V', ".--" ->  'W', "-..-" -> 'X',
    "-.--" -> 'Y', "--.." -> 'Z')

  val morze2 = morze.map(_.swap)

  def toMorze(word: String) = word.foldLeft("")(_ + morze2(_))

  def wordPair(word: String, seq: String) = {
    val rmword = toMorze(word).replace(".", "\\.")

    val pattern: Pattern = Pattern.compile(rmword)
    val matcher: Matcher = pattern.matcher(seq)

    while (matcher.find) {
      val start = matcher.start
      val end = matcher.end

      posStartMap.get(start) match {
        case None => posStartMap.put(start, Map(end -> 1))
        case Some(wordInfo) => posStartMap.put(start, wordInfo + (end -> (wordInfo.getOrElse(end, 0) + 1)))
      }
    }
  }

  def allPaths(from: Int, to: Int): Long = {
    val stack = mutable.Stack[(Int, Int)]()
    stack.push(from -> 1)
    var mul = 1
    var sum: Long = 0

    while (stack.nonEmpty) {
      val (newIndex, newMul) = stack.pop
      mul = mul * newMul
      sum = posStartMap.get(newIndex) match {
        case None =>
          val nm = mul
          mul = 1
          if (newIndex == to) sum + nm else sum
        case Some(info) =>
          stack.pushAll(info)
          sum
      }
    }
    sum
  }

  val seq = readLine
  val dictSize = readInt
  for (i <- 0 until dictSize) wordPair(readLine, seq)
  println(allPaths(0, seq.length))
}