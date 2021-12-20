package aoc2021

// Solution to https://adventofcode.com/2021/day/5

object  Day7 {
  def parseInput(input: String): Seq[Int] =
    input.split(",").map(_.toInt)

  def findOptimal(nums: Seq[Int], computeScore: Int => Int): Int =
    val min = nums.min
    val max = nums.max

    (min to max).map(computeScore).min

  def findOptimalPart1(nums: Seq[Int]): Int =
    findOptimal(nums, pos => (for n <- nums yield (n - pos).abs).sum)

  def findOptimalPart2(nums: Seq[Int]): Int =
    val computeTripCost = (distance: Int) => (distance.toFloat / 2 * (1 + distance)).toInt
    val computeScore = (pos: Int) => (for n <- nums yield computeTripCost((n - pos).abs)).sum
    findOptimal(nums, computeScore)

  def main(args: Array[String]): Unit = {
    val nums = parseInput(Utils.readInput("day7.txt"))

    println(s"Solution for Part 1 = ${findOptimalPart1(nums)}")
    println(s"Solution for Part 2 = ${findOptimalPart2(nums)}")
  }
}
