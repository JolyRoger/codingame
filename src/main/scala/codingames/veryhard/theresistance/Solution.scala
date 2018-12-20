package codingames.veryhard.theresistance

object Solution extends App {

  def MAX_SYMBOL_SIZE = 4
  def mydict = List("HELL", "HELLO", "OWORLD", "WORLD", "TEST")
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

  def allPossibleStartingWords(seq: String) = {
    def findWords(wordCandidate: String, seq: String, found: List[String]): List[String] = {
        allPossibleStartingLetters(seq).flatMap(letter => {
        val newWordCandidate = wordCandidate + letter._1
        if (!mydict.exists(_ startsWith newWordCandidate)) found else
        if (mydict.contains(newWordCandidate)) findWords(newWordCandidate, seq.drop(letter._2), newWordCandidate :: found)
          else findWords(newWordCandidate, seq.drop(letter._2), found)
      }).distinct
    }
    findWords("", seq, List.empty[String])
  }

  val l = readLine
  val n = readInt
  Console.err.println(s"l=$l\tn=$n")

  val dict = for (i <- 0 until n) yield readLine
  dict.foreach(word => Console.err.print(s"$word "))

  println("1")
}