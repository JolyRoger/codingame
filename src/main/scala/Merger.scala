import java.io.{BufferedWriter, File, FileWriter}

import scala.io.Source

object Merger extends App {

  val PROJECT = "src/main/scala"
  val PATH = "/codingames/challenge/iceandfire"
  val SOURCE = s"$PROJECT$PATH/src"
  val DEST = s"$PROJECT$PATH/Player.scala"
//  val DEST = s"$PROJECT$PATH/dest.txt"
  val REPL_SIGN = "///"
  val NEWSTR = "\n"

  val sourceDir = new File(s"$SOURCE")
  val destFile = new File(s"$DEST")

  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }

  def repl(str: String) = str.trim.startsWith(REPL_SIGN)

  val sourceFiles: Array[File] = recursiveListFiles(sourceDir).filterNot(_.isDirectory)
//  val sourceFiles: Array[File] = sourceDir.listFiles.filterNot(_.isDirectory)


  sourceFiles.foreach(println)


  val bufferedDest = Source.fromFile(destFile)
  val destLines = bufferedDest.getLines.toList
  val destReplLines = destLines.filter(repl)
  val destFileNames = destReplLines.map(_.split(REPL_SIGN).filterNot(_.trim.isEmpty))
  val nameContentMap = sourceFiles.map(file => (REPL_SIGN + file.getName,
    processLine(Source.fromFile(file).getLines.toList).mkString("", NEWSTR, "") )).toMap

  val destContent = processLine(destLines).map(str =>
    if (str.trim.startsWith(REPL_SIGN)) nameContentMap(str.trim) else str).mkString("", NEWSTR, NEWSTR)

  writeToFile(destFile, destContent)

  def processLine(lines: List[String]) = {
    lines.filterNot(str => str.trim.startsWith("package") ||
      (str.trim.startsWith("import") && !str.trim.startsWith("import scala")))
  }
  def writeToFile(file: File, text: String) = {
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(text)
    bw.close()
  }
}
