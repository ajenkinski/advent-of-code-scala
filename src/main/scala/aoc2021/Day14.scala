package aoc2021

// Solution to https://adventofcode.com/2021/day/14

object Day14 extends AOCDay {
  case class Problem(template: String, rules: Map[String, Char])

  override type InputT = Problem

  override def parseInput(input: String): Problem =
    val parts = input.trim.split(raw"\n\s*\n")
    val template = parts(0)

    val ruleRx = raw"(..) -\> (.)".r
    val rules = parts(1).linesIterator.map { line =>
      line match {
        case ruleRx(pair, element) => (pair, element(0))
        case _ => throw new Exception(s"Couldn't parse rule '$line''")
      }
    }.toMap

    Problem(template, rules)

  /** Run numSteps steps of the pair insertion rule.
   * Returns a (pairCounts, letterCounts) tuple, where pairCounts specifies the counts of each pair in the
   * final string, and letterCounts is the count of each letter in the final string.
   */
  def run(problem: Problem, numSteps: Int): (Map[String, Long], Map[Char, Long]) =
    val allLetters = (problem.template + problem.rules.keys.mkString + problem.rules.values.mkString).toSet
    val allPairs = (for l1 <- allLetters; l2 <- allLetters yield s"$l1$l2").toSet

    // make a map from letter pairs to the count of how many times the pair occurs in the current string
    val pairCounts = problem.template.sliding(2).foldLeft(allPairs.map((_, 0L)).toMap) { (counts, pair) =>
      counts.updated(pair, counts(pair) + 1)
    }

    // make a map from letter to number of occurrences of leter
    val letterCounts = problem.template.foldLeft(allLetters.map((_, 0L)).toMap) { (counts, letter) =>
      counts.updated(letter, counts(letter) + 1)
    }

    // Update pairCounts and letterCounts for numSteps
    val (finalPairCounts, finalLetterCounts) = (1 to numSteps).foldLeft((pairCounts, letterCounts)) {
      case ((stepPairCounts, letterCounts), step) =>
        // update pairCounts and letterCounts by applying each pair insertion rule
        stepPairCounts.filter(_._2 > 0).foldLeft((stepPairCounts, letterCounts)) {
          case ((pairPairCounts, pairLetterCounts), (pair, count)) =>
            val newLetter = problem.rules(pair)

            // two new pairs are created by inserting the new letter
            val (newPair1, newPair2) = (s"${pair(0)}${newLetter}", s"${newLetter}${pair(1)}")

            // If inserting a C between all ABs, then the count of AB goes down by count, and the
            // count of AC and CB goes up by count
            val pairUpdates = Seq((pair, -count), (newPair1, count), (newPair2, count))
            val nextPairCounts = pairUpdates.foldLeft(pairPairCounts) { case (counts, (pair, count)) =>
              counts.updated(pair, counts(pair) + count)
            }

            // count for newLetter goes up by count
            val nextLetterCounts = pairLetterCounts.updated(newLetter, pairLetterCounts(newLetter) + count)

            (nextPairCounts, nextLetterCounts)
        }
    }

    (finalPairCounts, finalLetterCounts)

  def solve(problem: Problem, numSteps: Int): Long =
    val (pairCounts, letterCounts) = run(problem, numSteps)
    letterCounts.values.max - letterCounts.values.min

  def solvePart1(problem: Problem): Long =
    solve(problem, 10)

  def solvePart2(problem: Problem): Long =
    solve(problem, 40)

  def main(args: Array[String]): Unit =
    val input = parseInputFile("day14.txt")

    println(s"Solution to part 1 = ${solvePart1(input)}")
    println(s"Solution to part 2 = ${solvePart2(input)}")
}
