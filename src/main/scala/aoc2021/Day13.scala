package aoc2021

// Solution to https://adventofcode.com/2021/day/13

object Day13 extends AOCDay {
  override type InputT = Problem

  type Sheet = Set[(Int, Int)]

  enum Direction:
    case X, Y

  case class Instruction(direction: Direction, at: Int)
  case class Problem(points: Sheet, instructions: Seq[Instruction])

  override def parseInput(input: String): Problem =
    // points and instructions are separated by a blank line
    val parts = input.split(raw"\n\s*\n")

    val pointRx = raw"(\d+),(\d+)".r
    val points = parts(0).linesIterator.map { line =>
      line match {
        case pointRx(x, y) => (x.toInt, y.toInt)
        case _ => throw new Exception(s"Couldn't parse point: '$line''")
      }
    }.toSet

    val instructionRx = raw"fold along (x|y)=(\d+)".r
    val instructions = parts(1).linesIterator.map { line =>
      line match {
        case instructionRx(direction, atStr) => Instruction(Direction.valueOf(direction.toUpperCase), atStr.toInt)
        case _ => throw new Exception(s"Couldn't parse instruction: '$line''")
      }
    }.toSeq

    Problem(points, instructions)

  def foldSheet(sheet: Sheet, fold: Instruction): Sheet =
    fold.direction match {
      case Direction.X =>
        val (left, right) = sheet.partition { case (x, y) => x <= fold.at }
        left | right.map { case (x, y) => (x - 2 * (x - fold.at), y) }
      case Direction.Y =>
        val (top, bottom) = sheet.partition { case (x, y) => y <= fold.at }
        top | bottom.map { case (x, y) => (x, y - 2 * (y - fold.at)) }
    }

  def displaySheet(sheet: Sheet): Unit =
    val numRows = sheet.map(_._2).max + 1
    val numCols = sheet.map(_._1).max + 1

    for y <- 0 until numRows do
      println((0 until numCols).map(x => if sheet((x, y)) then '#' else '.').mkString)

  def solvePart1(problem: Problem): Int =
    foldSheet(problem.points, problem.instructions(0)).size

  def solvePart2(problem: Problem): Unit =
    val finalSheet = problem.instructions.foldLeft(problem.points){ (sheet, instruction) =>
      foldSheet(sheet, instruction)
    }

    displaySheet(finalSheet)

  def main(args: Array[String]): Unit =
    val input = parseInputFile("day13.txt")

    println(s"Solution to part 1 = ${solvePart1(input)}")
    println(s"Solution to part 2:")
    solvePart2(input)

}
