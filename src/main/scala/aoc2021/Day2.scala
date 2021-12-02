package aoc2021

// Solution to https://adventofcode.com/2021/day/2

object Day2 {
  enum Instruction:
    case Forward(distance: Int)
    case Down(distance: Int)
    case Up(distance: Int)
    
  import Instruction._

  def parseInput(input: String): Seq[Instruction] =
    input.linesIterator
      .map(_.split(" +"))
      .map({ case Array(name, distance) => name match {
        case "forward" => Forward(distance.toInt)
        case "down" => Down(distance.toInt)
        case "up" => Up(distance.toInt)
        case _ => throw new IllegalArgumentException(s"Unexpected instruction: $name")
      }
      })
      .toSeq

  def solvePart1(instructions: Seq[Instruction]): Int =
    val (position, depth) = instructions.foldLeft((0, 0)) {
      case ((position, depth), instruction) => instruction match {
        case Forward(dist) => (position + dist, depth)
        case Up(dist) => (position, depth - dist)
        case Down(dist) => (position, depth + dist)
      }
    }

    position * depth

  def solvePart2(instructions: Seq[Instruction]): Int =
    val (aim, position, depth) = instructions.foldLeft((0, 0, 0)) {
      case ((aim, position, depth), instruction) => instruction match {
        case Forward(dist) => (aim, position + dist, depth + aim * dist)
        case Up(dist) => (aim - dist, position, depth)
        case Down(dist) => (aim + dist, position, depth)
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
