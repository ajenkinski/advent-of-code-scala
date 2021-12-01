package aoc2021

object Utils {
  def readInput(fileName: String): String =
    io.Source.fromInputStream(getClass.getResourceAsStream(fileName)).mkString.trim
}