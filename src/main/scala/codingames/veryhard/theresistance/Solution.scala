package codingames.veryhard.theresistance

object Solution extends App {

  val wordMap: scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map.empty[String, Int]

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

  def wordPair(word: String) = {
    val mword = toMorze(word)
    if (wordMap.contains(mword)) {
      wordMap.put(mword, wordMap(mword) + 1)
    } else {
      wordMap.put(mword, 1)
    }
  }

  def find(seq: Stream[Char]): Int = {
    if (seq.isEmpty) 1 else {
      val res = for {
        indexedSeq <- seq.zipWithIndex
        candidate = seq.take(indexedSeq._2 + 1).foldLeft("")(_ + _)
        if wordMap.contains(candidate)
      } yield wordMap(candidate) * find(seq.drop(indexedSeq._2 + 1))
      res.sum
    }
  }

  val seq = readLine.toStream
  val dictSize = readInt
  for (i <- 0 until dictSize) wordPair(readLine)

  println(find(seq))
}