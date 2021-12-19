package aoc2021

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

  def applyRule(template: String, rules: Map[String, Char]): String =
    template.slice(0, 1) + template.sliding(2).map { pair => s"${rules(pair)}${pair(1)}" }.mkString

  def solve(problem: Problem, numSteps: Int): Int =
    val result = (1 to numSteps).foldLeft(problem.template) { (polymer, _) =>
      applyRule(polymer, problem.rules)
    }

    val counts = result.groupBy(x => x).values.map(_.length).toSeq
    counts.max - counts.min

  def solvePart1(problem: Problem): Int =
    solve(problem, 10)

  def solvePart2(problem: Problem): Int =
    solve(problem, 40)

  def main(args: Array[String]): Unit =
    val input = parseInputFile("day14.txt")

    println(s"Solution to part 1 = ${solvePart1(input)}")
    println(s"Solution to part 2 = ${solvePart2(input)}")
}
