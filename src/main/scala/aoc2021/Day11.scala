package aoc2021

// Solution to https://adventofcode.com/2021/day/9

object Day11 extends AOCDay {
  override type InputT = Grid

  type Grid = Array[Array[Int]]

  val neighborOffsets = Seq((-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1))

  extension (g: Grid)
    def numRows: Int = g.length
    def numCols: Int = g(0).length
    def rowIndices: Range = g.indices
    def colIndices: Range = g(0).indices
    def coords: Iterable[(Int, Int)] = new Iterable[(Int, Int)] {
      override def iterator: Iterator[(Int, Int)] =
        for r <- rowIndices.iterator; c <- colIndices.iterator yield (r, c)
    }

    def definedAt(row: Int, col: Int): Boolean = g.isDefinedAt(row) && g(row).isDefinedAt(col)
    def neighborCoords(row: Int, col: Int): Seq[(Int, Int)] =
      neighborOffsets
        .map { case (dr, dc) => (row + dr, col + dc) }
        .filter { case (nr, nc) => definedAt(nr, nc) }

    def display: Unit =
      println(g.map(_.mkString).mkString("\n"))

    def deepClone: Grid = g.map(_.clone)

  override def parseInput(input: String): Grid =
    input.linesIterator
      .map(line => line.split("").map(_.toInt).toArray)
      .toArray

  // single steps the grid, and returns the number of flashes that occurred during the step
  def singleStepGrid(grid: Grid): Int =
    var flashed = Set.empty[(Int, Int)]

    // first increment all elements by 1
    for (row, col) <- grid.coords do
      grid(row)(col) += 1

    while
      var prevFlashed = flashed
      for (row, col) <- grid.coords do
        if grid(row)(col) >= 10 && !flashed.contains((row, col)) then
          flashed += (row, col)
          for (nrow, ncol) <- grid.neighborCoords(row, col) do
            grid(nrow)(ncol) += 1
      prevFlashed ne flashed
    do ()

    // finally set any flashed locations back to 0
    for (row, col) <- grid.coords do
      if grid(row)(col) >= 10 then
        grid(row)(col) = 0

    flashed.size

  def solvePart1(grid: Grid): Int =
    val gridCopy = grid.deepClone
    var totalFlashed = 0

    for _ <- 1 to 100 do
      totalFlashed += singleStepGrid(gridCopy)

    totalFlashed

  def solvePart2(grid: Grid): Int =
    val gridCopy = grid.deepClone

    var index = 0
    while !gridCopy.coords.forall { case (row, col) => gridCopy(row)(col) == 0 } do
      singleStepGrid(gridCopy)
      index += 1

    index

  def main(args: Array[String]): Unit =
    val input = parseInputFile("day11.txt")

    println(s"Solution to part 1 = ${solvePart1(input)}")
    println(s"Solution to part 2 = ${solvePart2(input)}")
}
