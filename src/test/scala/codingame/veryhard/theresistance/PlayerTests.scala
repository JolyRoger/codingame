package codingame.veryhard.theresistance

import codingames.veryhard.theresistance.Solution
import org.scalatest.FlatSpec

class SolutionTests extends FlatSpec {

  val seq = "......-...-..---.-----.-..-..-.."
  val dictNum = 5
  val dict = List("HELL", "HELLO", "OWORLD", "WORLD", "TEST")

  "A solution" should "find all possible starting letters" in {
    val startingLetters = Solution.allPossibleStartingLetters(seq)
    Console.err.println(s"letters: $startingLetters")
    assert(startingLetters == List(('E',1), ('I',2), ('S',3), ('H',4)))
  }
  "A solution" should "find all possible starting words" in {
    val startingWords = Solution.allPossibleStartingWords(seq)
    Console.err.println(s"words: $startingWords")
  }
}