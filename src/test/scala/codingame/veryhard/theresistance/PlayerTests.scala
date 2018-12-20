package codingame.veryhard.theresistance

import codingames.veryhard.theresistance.Solution
import org.scalatest.FlatSpec

class SolutionTests extends FlatSpec {

  val seq = "......-...-..---.-----.-..-..-.."
  val seq2 = "-.-"
  val dictNum = 5
  val dict = List("HELL", "HELLO", "OWORLD", "WORLD", "TEST")
  val dict2 = List("A", "B", "C", "HELLO", "K", "WORLD")

  "A solution" should "find all possible starting letters" in {
    val startingLetters = Solution.allPossibleStartingLetters(seq)
    Console.err.println(s"letters: $startingLetters")
    assert(startingLetters == List(('E',1), ('I',2), ('S',3), ('H',4)))
  }

  "A solution" should "find all possible starting words" in {
    val startingWords = Solution.allPossibleStartingWords(seq, dict)
    val startingWords2 = Solution.allPossibleStartingWords(seq2, dict2)
    Console.err.println(s"words: $startingWords")
    Console.err.println(s"words2: $startingWords2")
    assert(startingWords2 == List(("K", 3)))
    assert(startingWords == List(("HELL", 13), ("HELLO", 16)))
  }

  "A solution" should "find a message" in {
    val message = Solution.message(seq, dict)
    assert(message.size == 2)
    Console.err.println(s"message: $message")
  }
}