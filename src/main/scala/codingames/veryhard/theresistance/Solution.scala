//package codingames.veryhard.theresistance

object Solution extends App {

  def MAX_SYMBOL_SIZE = 4

  def morze: Map[String, Char] = Map(
    ".-" ->   'A', "-..." -> 'B', "-.-." -> 'C', "-.." ->  'D',
    "." ->    'E', "..-." -> 'F', "--." ->  'G', "...." -> 'H',
    ".." ->   'I', ".---" -> 'J', "-.-" ->  'K', ".-.." -> 'L',
    "--" ->   'M', "-." ->   'N', "---" ->  'O', ".--." -> 'P',
    "--.-" -> 'Q', ".-." ->  'R', "..." ->  'S', "-" ->    'T',
    "..-" ->  'U', "...-" -> 'V', ".--" ->  'W', "-..-" -> 'X',
    "-.--" -> 'Y', "--.." -> 'Z')

  def allPossibleStartingLetters(seq: String) = seq.take(MAX_SYMBOL_SIZE).indices.map(index => {
      morze.get(seq.take(index + 1).toString) match {
        case Some(s) => (s, index + 1)
        case None => ('\0', index + 1)
      }
    }).filterNot(_._1 == '\0').toList

  def allPossibleStartingWords(seq: String, dict: List[String]) = {
    def findWords(wordCandidate: String, seq: String, found: List[(String, Int)], offset: Int): List[(String, Int)] = {
      if (seq.isEmpty) found else
      allPossibleStartingLetters(seq).flatMap(letter => {
        val newWordCandidate = wordCandidate + letter._1
        if (!dict.exists(_ startsWith newWordCandidate)) found else
        if (dict.contains(newWordCandidate)) findWords(newWordCandidate, seq.drop(letter._2), (newWordCandidate, offset + letter._2) :: found, offset + letter._2)
          else findWords(newWordCandidate, seq.drop(letter._2), found, offset + letter._2)
      }).distinct
    }
    findWords("", seq, List.empty[(String, Int)], 0)
  }

  def message(seq: String, dict: List[String]): List[String] = {
    def findMessage(seq: String, found: String, offset: Int): List[String] = {
      if (seq.isEmpty) List(found) else
      allPossibleStartingWords(seq, dict).flatMap(word => findMessage(seq.drop(word._2), found + " " + word._1, offset + word._1.length)
      )
    }
    findMessage(seq, "", 0)
  }

  val seq = readLine
  val dictSize = readInt
  Console.err.println(s"seq: $seq\tdictSize: $dictSize")

  val dict = (for (i <- 0 until dictSize) yield readLine).toList
  dict.foreach(word => Console.err.print(s"$word "))

  println(message(seq, dict).size)
}