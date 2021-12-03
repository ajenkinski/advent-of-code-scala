package aoc2021

import scala.annotation.tailrec

// Solution to https://adventofcode.com/2021/day/3

object Day3 {
  def parseInput(input: String): Seq[Seq[Int]] =
    // Each line is a binary number.  Convert each number to a sequence of digits
    input.linesIterator
      .map(line => line.map(_.toInt - '0'.toInt))
      .toSeq

  def solvePart1(nums: Seq[Seq[Int]]): Int =
    val numBits = nums(0).length

    // sum the columns in nums
    val sums = nums.transpose.map(_.sum)

    // For gamma, majority wins for each bit
    val gammaStr = sums.map(n => if n >= nums.length / 2 then 1 else 0).mkString
    val gamma = Integer.parseInt(gammaStr, 2)

    // epsilon is the opposite
    val mask = ~(Int.MaxValue << numBits)
    val epsilon = ~gamma & mask

    gamma * epsilon

  def solvePart2Helper(nums: Seq[Seq[Int]], filterPos: Int, keepLonger: Boolean): Int =
    if nums.length > 1 then
      val (ones, zeros) = nums.partition(s => s(filterPos) == 1)
      val filtered = if keepLonger then
        if ones.length >= zeros.length then ones else zeros
      else
        if zeros.length <= ones.length then zeros else ones

      solvePart2Helper(filtered, filterPos + 1, keepLonger)
    else
      Integer.parseInt(nums(0).mkString, 2)

  def solvePart2(nums: Seq[Seq[Int]]): Int =
    val oxygenRating = solvePart2Helper(nums, 0, true)
    val co2Rating = solvePart2Helper(nums, 0, false)

    oxygenRating * co2Rating

  def main(args: Array[String]): Unit =
    val nums = parseInput(Utils.readInput("day3.txt"))

    val part1Solution = solvePart1(nums)
    println(s"Part 1 solution = $part1Solution")

    val part2Solution = solvePart2(nums)
    println(s"Part 2 solution = $part2Solution")


}
