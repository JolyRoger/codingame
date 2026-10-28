package codingames.easy.decoder

import math._
import scala.util._
import scala.io.StdIn._

/**
 * Decode and execute the MiniCPU bytecode program.
 **/
object Solution extends App {
//  val program = readLine // Space-separated hex bytes representing CPU instructions
  case class Instr(op: String, reg: String, v: String)

  val registers = Array(0,0,0,0)

  val program = "01 00 05 01 01 0A 03 00 01 FF"
  val data = program.split("\\s")
  var operate = true
  val it = data.iterator

  while (operate) {
    val command = it.next
    command match {
      case "01" =>
        val x = it.next.toInt
        val v = it.next
        // println(s"x=$x v=$v")
        registers(x) = Integer.parseInt(v, 16) % 256
      case "02" =>
        val x = it.next.toInt
        val y = it.next.toInt
        registers(x) = (registers(x) + registers(y)) % 256
      case "03" =>
        val x = it.next.toInt
        val y = it.next.toInt
        registers(x) = if (registers(x) >= registers(y)) registers(x) - registers(y)
                       else 256 - (registers(y) - registers(x))
      case "04" =>
        val x = it.next.toInt
        val y = it.next.toInt
        registers(x) = (registers(x) * registers(y)) % 256
      case "05" =>
        val x = it.next.toInt
        registers(x) = (registers(x) + 1) % 256
      case "06" =>
        val x = it.next.toInt
        registers(x) = if (registers(x) == 0) 255 else registers(x) - 1
      case "FF" => operate = false
    }
  }

  println(s"${registers(0)}\n${registers(1)}\n${registers(2)}\n${registers(3)}")
}