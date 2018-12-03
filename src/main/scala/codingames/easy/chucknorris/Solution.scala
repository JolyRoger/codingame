package codingames.easy.chucknorris

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Solution extends App {
    val message = readLine

    def toOctBinaryString(ch: Int) : String = {
        val res = ch.toBinaryString
        val zero = for (i <- 0 until 7 - res.length) yield "0"
        if (zero.isEmpty) res else zero.reduce(_+_) + res
    }
    val res = message.map(s => toOctBinaryString(s.toInt)).reduce(_+_).split("(?<=(.))(?!\\1)").map {
        block => {
            val first = if (block.startsWith("0")) "00" else "0"
            val second = block.replaceAll(".", "0")
            first + " " + second + " "
        }
    }.reduce(_+_).trim

    // Write an action using println
    // To debug: Console.err.println("Debug messages...")

    println(res)
}
