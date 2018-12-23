package codingame.veryhard.theresistance

import java.util.regex.{Matcher, Pattern}
import codingames.veryhard.theresistance.Solution
import org.scalatest.FlatSpec

class SolutionTests extends FlatSpec {
  val seq = Array("......-...-..---.-----.-..-..-..", // 0
                  "-.-", // 1
                  "-....--.-.-.-", // 2
                  ".", // 3
                  "..", // 4
                  "--.-------..") // 5
  val dict = Array(List("HELL", "HELLO", "OWORLD", "WORLD", "TEST"),
                   List("A", "B", "C", "HELLO", "K", "WORLD"),
                   List("BAC", "DUC", "AQUA", "BANN", "K", "BACK", "BANNK"),
                   List("E"),
                   List("I", "E", "EE"),
                   List("GOD", "GOOD", "MORNING", "G", "HELLO"))
  val res = Array(2, 1, 5, 1, 3, 1)

  val wordMap: scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map.empty[String, Int]
  // key -> start position, value -> List(<offset>, <multiplier>, <mword>)
  val posStartMap: scala.collection.mutable.Map[Int, Map[Int, Int]] = scala.collection.mutable.Map.empty[Int, Map[Int, Int]]

  "A solution" should "solve the problem" in {
    def wordPair(word: String, index: Int) {
      val mword = Solution.toMorze(word)
      val rmword = mword.replace(".", "\\.")

      val pattern: Pattern = Pattern.compile(rmword)
      val matcher: Matcher = pattern.matcher(seq(index))

      while (matcher.find) {
        val start = matcher.start
        val end = matcher.end

        posStartMap.get(start) match {
          case None => posStartMap.put(start, Map(end -> 1))
          case Some(wordInfo) => posStartMap.put(start, wordInfo + (end -> (wordInfo.getOrElse(end, 0) + 1)))
        }
      }
    }

    def find(seq: Stream[Char], index: Int): Int = {
        posStartMap.get(index) match {
        case Some(infoMap) => (for ((end, multiplier) <- infoMap) yield multiplier * find(seq.drop(end), end)).sum
        case None => if (seq.isEmpty) 1 else 0
      }
    }

    seq.indices.foreach(index => {
      posStartMap.clear
      for (i <- dict(index).indices) wordPair(dict(index)(i), index)
      val actualRes = find(seq(index).toStream, 0)
      Console.err.println(s"\nseq: ${seq(index)}")
      Console.err.println(s"dict: ${dict(index)}")
      Console.err.println(s"res: $actualRes")
      assert(actualRes == res(index))
    })
  }
}
