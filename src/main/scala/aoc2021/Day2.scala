package aoc2021

// Solution to https://adventofcode.com/2021/day/2

object Day2 {
  enum Instruction:
    case Forward(distance: Int)
    case Down(distance: Int)
    case Up(distance: Int)

  def parseInput(input: String): Seq[Instruction] =
    input.linesIterator
      .map(_.split(" +"))
      .map({ case Array(name, distance) => name match {
        case "forward" => Instruction.Forward(distance.toInt)
        case "down" => Instruction.Down(distance.toInt)
        case "up" => Instruction.Up(distance.toInt)
        case _ => throw new IllegalArgumentException(s"Unexpected instruction: $name")
      }
      })
      .toSeq

  def solvePart1(instructions: Seq[Instruction]): Int =
    val (position, depth) = instructions.foldLeft((0, 0)) {
      case ((position, depth), instruction) => instruction match {
        case Instruction.Forward(dist) => (position + dist, depth)
        case Instruction.Up(dist) => (position, depth - dist)
        case Instruction.Down(dist) => (position, depth + dist)
      }
    }

    position * depth

  def solvePart2(instructions: Seq[Instruction]): Int =
    val (aim, position, depth) = instructions.foldLeft((0, 0, 0)) {
      case ((aim, position, depth), instruction) => instruction match {
        case Instruction.Forward(dist) => (aim, position + dist, depth + aim * dist)
        case Instruction.Up(dist) => (aim - dist, position, depth)
        case Instruction.Down(dist) => (aim + dist, position, depth)
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
