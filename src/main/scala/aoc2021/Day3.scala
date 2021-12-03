package aoc2021

object Day3 {
  def parseInput(input: String): Seq[String] =
    input.linesIterator.toSeq

  def solvePart1(nums: Seq[String]): Int =
    val numBits = nums(0).length
    val sums = Array.ofDim[Int](numBits)

    for (num <- nums)
      for (i <- 0 to numBits - 1)
        if num(i) == '1' then
          sums(i) += 1

    var gamma = 0
    var epsilon = 0
    for (i <- 0 to numBits - 1)
      if sums(numBits - i - 1) >= nums.length / 2 then
        gamma |= 1 << i
      else
        epsilon |= 1 << i

    gamma * epsilon

  def solvePart2Helper(nums: Seq[String], keepLonger: Boolean): Int =
    var filtered = nums
    var filterPos = 0

    while filtered.length > 1 do
      val (ones, zeros) = filtered.partition(s => s.charAt(filterPos) == '1')
      if keepLonger then
        filtered = if ones.length >= zeros.length then ones else zeros
      else
        filtered = if zeros.length <= ones.length then zeros else ones

      filterPos += 1

    Integer.parseInt(filtered(0), 2)

  def solvePart2(nums: Seq[String]): Int =
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
