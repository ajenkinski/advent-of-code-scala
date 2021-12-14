package aoc2021

object Utils {
  def readInput(fileName: String): String =
    io.Source.fromInputStream(getClass.getResourceAsStream(fileName)).mkString.trim
}

trait AOCDay[T] {
  def parseInput(input: String): T
  
  def parseInputFile(inputFile: String): T =
    parseInput(Utils.readInput(inputFile))
}
