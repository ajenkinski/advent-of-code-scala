package aoc2021

// Solution to https://adventofcode.com/2021/day/3

object Day3 {
  def parseInput(input: String): Seq[Seq[Int]] =
    // Each line is a binary number.  Convert each number to a sequence of digits
    input.linesIterator
      .map(line => line.map(_.toInt - '0'.toInt))
      .toSeq

  def solvePart1(nums: Seq[Seq[Int]]): Int =
    val numBits = nums(0).length

    // sums(i) contains the number of 1 bits in position i of all nums
    val sums = nums.reduce {
      (a, b) => a.zip(b).map { case (n1, n2) => n1 + n2 }
    }

    // For gamma, majority wins for each bit
    val gammaStr = sums.map(n => if n >= nums.length / 2 then 1 else 0).mkString
    val gamma = Integer.parseInt(gammaStr, 2)

    // epsilon is the opposite
    val mask = ~(Int.MaxValue << numBits)
    val epsilon = ~gamma & mask

    gamma * epsilon

  def solvePart2Helper(nums: Seq[Seq[Int]], keepLonger: Boolean): Int =
    var filtered = nums
    var filterPos = 0

    while filtered.length > 1 do
      val (ones, zeros) = filtered.partition(s => s(filterPos) == 1)
      if keepLonger then
        filtered = if ones.length >= zeros.length then ones else zeros
      else
        filtered = if zeros.length <= ones.length then zeros else ones

      filterPos += 1

    Integer.parseInt(filtered(0).mkString, 2)

  def solvePart2(nums: Seq[Seq[Int]]): Int =
    val oxygenRating = solvePart2Helper(nums, true)
    val co2Rating = solvePart2Helper(nums, false)

    oxygenRating * co2Rating

  def main(args: Array[String]): Unit =
    val nums = parseInput(Utils.readInput("day3.txt"))

    val part1Solution = solvePart1(nums)
    println(s"Part 1 solution = $part1Solution")

    val part2Solution = solvePart2(nums)
    println(s"Part 2 solution = $part2Solution")


}
