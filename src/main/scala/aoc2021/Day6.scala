package aoc2021

// Solution to https://adventofcode.com/2021/day/5

object Day6 {
  // We don't need to keep track of the countdown timer for each individual fish.  It's enough to just keep track of
  // how many fish are at each time step.  Valid timer values are from 0 to 8, so we'll use a Vector as map from
  // timer value to fish count.  We need to use Long instead of Int to avoid overflow.
  type Timers = Vector[Long]

  def parseInput(input: String): Timers =
    val school = input.split(",").map(_.toInt)

    // create Vector where timers(i) is the number of fish with timer value i
    school.foldLeft(Vector.fill[Long](9)(0)) { (timers, timer) =>
      timers.updated(timer, timers(timer) + 1)
    }

  def stepOneDay(timers: Timers): Timers =
    val numZeroes +: rest = timers
    // All the zero timers go back to 6, and also they each spawn a new length 8 timer
    rest.updated(6, rest(6) + numZeroes) :+ numZeroes

  def solve(timers: Timers, numDays: Int): Long =
    val newTimers = (1 to numDays).foldLeft(timers) { (timers, _) => stepOneDay(timers) }
    newTimers.sum

  def main(args: Array[String]): Unit =
    val school = parseInput(Utils.readInput("day6.txt"))

    println(s"Part 1 solution = ${solve(school, 80)}")
    println(s"Part 2 solution = ${solve(school, 256)}")
}
