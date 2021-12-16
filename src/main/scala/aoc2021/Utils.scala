package aoc2021

object Utils {
  def readInput(fileName: String): String =
    io.Source.fromInputStream(getClass.getResourceAsStream(fileName)).mkString.trim
}

trait AOCDay {
  type InputT

  def parseInput(input: String): InputT

  def parseInputFile(inputFile: String): InputT =
    parseInput(Utils.readInput(inputFile))
}
