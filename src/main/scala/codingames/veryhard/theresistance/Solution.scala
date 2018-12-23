package codingames.veryhard.theresistance

import java.util.regex.{Matcher, Pattern}

object Solution extends App {

  val wordMap: scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map.empty[String, Int]
  val posStartMap: scala.collection.mutable.Map[Int, Map[Int, Int]] = scala.collection.mutable.Map.empty[Int, Map[Int, Int]]

  def morze: Map[String, Char] = Map(
    ".-" ->   'A', "-..." -> 'B', "-.-." -> 'C', "-.." ->  'D',
    "." ->    'E', "..-." -> 'F', "--." ->  'G', "...." -> 'H',
    ".." ->   'I', ".---" -> 'J', "-.-" ->  'K', ".-.." -> 'L',
    "--" ->   'M', "-." ->   'N', "---" ->  'O', ".--." -> 'P',
    "--.-" -> 'Q', ".-." ->  'R', "..." ->  'S', "-" ->    'T',
    "..-" ->  'U', "...-" -> 'V', ".--" ->  'W', "-..-" -> 'X',
    "-.--" -> 'Y', "--.." -> 'Z')

  def morze2 = morze.map(_.swap)

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

  def find(seq: String, index: Int): Int = {
    posStartMap.get(index) match {
      case Some(infoMap) => (for ((end, multiplier) <- infoMap) yield multiplier * find(seq.drop(end), end)).sum
      case None => if (seq.isEmpty) 1 else 0
    }
  }

  val seq = readLine
  val dictSize = readInt
  for (i <- 0 until dictSize) wordPair(readLine, seq)
  println(find(seq, 0))
}