package aoc2021

// Solution to https://adventofcode.com/2021/day/9

object Day11 extends AOCDay {
  override type InputT = Grid[Int]

  override def parseInput(input: String): InputT =
    Grid.from(input.linesIterator
      .map(line => line.split("").map(_.toInt).toSeq).toSeq)

  // single steps the grid, and returns the updated grid and the number of flashes that occurred during the step
  def singleStepGrid(grid: InputT): (InputT, Int) =
    var flashed = Set.empty[Coord]

    // first increment all elements by 1
    var grid2 = grid.mapGrid(_ + 1)

    while
      var prevFlashed = flashed
      for coord <- grid2.coords do
        if grid2(coord) > 9 && !flashed.contains(coord) then
          flashed += coord
          grid2 = grid2.allNeighbors(coord).foldLeft(grid2) { (grid, ncoord) =>
            grid.updated(ncoord, grid(ncoord) + 1)
          }
      prevFlashed ne flashed
    do ()

    // finally set any flashed locations back to 0
    grid2 = grid2.mapGrid(x => if x > 9 then 0 else x)

    (grid2, flashed.size)

  /** Alternate version of singleStepGrid using a recursive algorithm */
  def singleStepGridRecursive(grid: InputT): (InputT, Int) =
    def inner(grid: InputT, toFlash: List[Coord], flashed: Set[Coord]): (InputT, Int) =
      toFlash match {
        case Nil => (grid, flashed.size)
        case coord :: rest if !flashed(coord) =>
          val newFlashed = flashed + coord
          val neighbors = grid.allNeighbors(coord)
          val newGrid = neighbors.foldLeft(grid.updated(coord, 0)) { (grid, ncoord) =>
            grid.updated(ncoord, grid(ncoord) + 1)
          }
          val newToFlash = neighbors.filter(!flashed(_)).toList ::: toFlash
          inner(newGrid, newToFlash, newFlashed)
        case _ :: rest => inner(grid, rest, flashed)
      }

    val grid2 = grid.mapGrid(_ + 1)
    val initToFlash = grid2.coords.filter(grid2(_) > 9).toList
    inner(grid2, initToFlash, Set.empty)

  def solvePart1(grid: InputT): Int =
    Iterator.unfold(grid) {
      grid => Some(singleStepGridRecursive(grid).swap)
    }.take(100).sum

  def solvePart2(grid: InputT): Int =
    Iterator.unfold(grid) { grid =>
      if grid.coords.forall(c => grid(c) == 0) then
        None
      else
        Some((grid, singleStepGridRecursive(grid)._1))
    }.length

  def main(args: Array[String]): Unit =
    val input = parseInputFile("day11.txt")

    println(s"Solution to part 1 = ${solvePart1(input)}")
    println(s"Solution to part 2 = ${solvePart2(input)}")
}
