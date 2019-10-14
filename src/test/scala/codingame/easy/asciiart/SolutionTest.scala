package codingame.easy.asciiart

import org.scalatest.FlatSpec

class SolutionTest extends FlatSpec {
  "A solution" should "split to chunks" in {
    val in = "12345678".split("")
    val res = Solution.process(in, 4)
    assert(res sameElements Array("1234", "5678"))
  }
}
