package aoc2021

// Solution to https://adventofcode.com/2021/day/1

object Day1 {
  def parseInput(input: String): Seq[Int] =
    input.linesIterator.map(_.toInt).toSeq

  def solvePart1(entries: Seq[Int]): Int =
    entries
      .sliding(2)
      .count({ case Seq(a, b) => b > a })

  def solvePart2(entries: Seq[Int]): Int =
    entries
      .sliding(3)
      .map(_.sum)
      .sliding(2)
      .count({ case Seq(a, b) => b > a })

  def main(args: Array[String]): Unit =
    val entries = parseInput(Utils.readInput("day1.txt"))

    val part1Solution = solvePart1(entries)
    println(s"Part 1 solution = $part1Solution")

    val part2Solution = solvePart2(entries)
    println(s"Part 2 solution = $part2Solution")
}
