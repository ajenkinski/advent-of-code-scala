package aoc2021

// Solution to https://adventofcode.com/2021/day/9

object Day9 extends AOCDay {
  override type InputT = Seq[Seq[Int]]

  override def parseInput(input: String): InputT =
    input.linesIterator.map(line => line.split("").map(_.toInt).toSeq).toSeq

  val neighborOffsets = Seq((-1, 0), (0, 1), (1, 0), (0, -1))

  def neighborCoords(grid: InputT, row: Int, col: Int): Seq[(Int, Int)] =
    neighborOffsets
      .map { case (dr, dc) => (row + dr, col + dc) }
      .filter { case (r, c) => grid.isDefinedAt(r) && grid(0).isDefinedAt(c) }

  /** Return the low points on grid as (rowNum, colNum) tuples */
  def lowPoints(grid: InputT): Iterable[(Int, Int)] =
    val isLowPoint = (rowNum: Int, colNum: Int) => {
      val elem = grid(rowNum)(colNum)
      neighborCoords(grid, rowNum, colNum).forall { case (nr, nc) => grid(nr)(nc) > elem }
    }

    for {
      rowNum <- grid.indices
      colNum <- grid(0).indices
      if isLowPoint(rowNum, colNum)
    } yield (rowNum, colNum)

  def solvePart1(grid: InputT): Int =
    lowPoints(grid)
      .map({ case (row, col) => grid(row)(col) + 1 })
      .sum

  def solvePart2(grid: InputT): Int =
  // The problem description says that 9s don't belong to any basin, and all other locations belong to exactly one
  // basin, and that every basin has a low point.  Together, this means we can count on basins being surrounded by
  // 9s, and by traversing outward from each low point, we can find the size of each basin.
    42

  def main(args: Array[String]): Unit =
    val grid = parseInputFile("day9.txt")

    println(s"Solution for Part 1 = ${solvePart1(grid)}")
}
