package codingames.veryhard.theresistance

import java.util.regex.Pattern

object Solution extends App {

  val posStartMap = scala.collection.mutable.Map.empty[Int, Map[Int, Int]]

  def morze: Map[Char, String] = Map(
    'A' -> ".-",   'B' -> "-...", 'C' -> "-.-.", 'D' ->  "-..",
    'E' -> "." ,   'F' -> "..-.", 'G' -> "--.",  'H' -> "....",
    'I' -> "..",   'J' -> ".---", 'K' -> "-.-",  'L' -> ".-..",
    'M' -> "--",   'N' -> "-.",   'O' ->  "---", 'P' -> ".--.",
    'Q' -> "--.-", 'R' -> ".-.",  'S' ->  "...", 'T' -> "-",
    'U' -> "..-",  'V' -> "...-", 'W' ->  ".--", 'X' -> "-..-",
    'Y' -> "-.--", 'Z' -> "--..")

  def toMorze(word: String) = word.foldLeft("")(_ + morze(_))

  def wordPair(word: String, seq: String) = {
    val matcher = Pattern.compile(toMorze(word).replace(".", "\\.")).matcher(seq)

    while (matcher.find) {
      val start = matcher.start
      val end = matcher.end

      posStartMap.get(start) match {
        case None => posStartMap.put(start, Map(end -> 1))
        case Some(wordInfo) => posStartMap.put(start, wordInfo + (end -> (wordInfo.getOrElse(end, 0) + 1)))
      }
    }
  }

  def find(seq: String) = {
    val pathArray = Array.fill[Long](seq.length + 1)(0)
    pathArray(pathArray.length - 1) = 1

    posStartMap.keys.toList.sortWith(_ > _).foreach(key => {
      val info = posStartMap(key)
      pathArray(key) = info.keys.toList.map(innerKey => pathArray(innerKey) * info(innerKey)).sum
    })
    pathArray(0)
  }

  val seq = readLine
  val dictSize = readInt
  for (i <- 0 until dictSize) wordPair(readLine, seq)
  println(find(seq))
}