package aoc2021

// Solution to https://adventofcode.com/2021/day/5

object Day5 {
  case class Point(x: Int, y: Int)

  case class Line(start: Point, end: Point)

  def parseInput(input: String): Seq[Line] =
    val linePat = raw"(\d+),(\d+)\s*-\>\s*(\d+),(\d+)".r
    input.linesIterator.map { line =>
      line match {
        case linePat(x1, y1, x2, y2) => Line(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
      }
    }.toSeq

  /**
   * Solver for parts 1 and 2.  Draws lines on a grid, and returns the count of grid cells with more than one line
   * crossing them.
   */
  def solve(lines: Seq[Line]): Int =
    // Represent grid as a sparse map, where keys are Points, and values are number of lines intersecting point
    val grid: Map[Point, Int] = lines.foldLeft((Map.empty[Point, Int])) { (g, line) =>
      // take advantage of the fact that we know all the lines are vertical, horizontal or diagonal
      val dx = math.signum(line.end.x - line.start.x)
      val dy = math.signum(line.end.y - line.start.y)
      val numPoints = math.max(math.abs(line.end.x - line.start.x), math.abs(line.end.y - line.start.y)) + 1

      (0 to numPoints - 1).foldLeft(g) { (g, i) =>
        g.updatedWith(Point(line.start.x + i * dx, line.start.y + i * dy)) {
          case Some(count) => Some(count + 1)
          case None => Some(1)
        }
      }
    }

    grid.values.count(_ > 1)

  def main(args: Array[String]): Unit =
    val lines = parseInput(Utils.readInput("day5.txt"))

    // part 1 only uses vertical and horizontal lines
    val part1Solution = solve(lines.filter(line => line.start.x == line.end.x || line.start.y == line.end.y))
    println(s"Part 1 solution = $part1Solution")

    val part2Solution = solve(lines)
    println(s"Part 2 solution = $part2Solution")
}
