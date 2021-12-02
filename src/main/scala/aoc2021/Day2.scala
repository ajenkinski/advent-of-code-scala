package aoc2021

// Solution to https://adventofcode.com/2021/day/2

object Day2 {
  enum Instruction:
    case Forward(amount: Int)
    case Down(amount: Int)
    case Up(amount: Int)

  import Instruction._

  def parseInput(input: String): Seq[Instruction] =
    input.linesIterator
      .map(_.split(" +"))
      .map({ case Array(name, amountStr) => {
        val amount = amountStr.toInt
        name match {
          case "forward" => Forward(amount)
          case "down" => Down(amount)
          case "up" => Up(amount)
          case _ => throw new IllegalArgumentException(s"Unexpected instruction: $name")
        }
      }
      })
      .toSeq

  def solvePart1(instructions: Seq[Instruction]): Int =
    val (position, depth) = instructions.foldLeft((0, 0)) {
      case ((position, depth), instruction) => instruction match {
        case Forward(amt) => (position + amt, depth)
        case Up(amt) => (position, depth - amt)
        case Down(amt) => (position, depth + amt)
      }
    }

    position * depth

  def solvePart2(instructions: Seq[Instruction]): Int =
    val (aim, position, depth) = instructions.foldLeft((0, 0, 0)) {
      case ((aim, position, depth), instruction) => instruction match {
        case Forward(amt) => (aim, position + amt, depth + aim * amt)
        case Up(amt) => (aim - amt, position, depth)
        case Down(amt) => (aim + amt, position, depth)
      }
    }

    position * depth

  def main(args: Array[String]): Unit =
    val instructions = parseInput(Utils.readInput("day2.txt"))

    val part1Solution = solvePart1(instructions)
    println(s"Part 1 solution = $part1Solution")

    val part2Solution = solvePart2(instructions)
    println(s"Part 2 solution = $part2Solution")
}
