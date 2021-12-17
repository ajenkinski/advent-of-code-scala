package aoc2021

import aoc2021.Day9.Day9Graph

// Solution to https://adventofcode.com/2021/day/9

object Day9 extends AOCDay {
  override type InputT = Grid[Int]

  override def parseInput(input: String): InputT =
    Grid.from(input.linesIterator.map(line => line.split("").map(_.toInt).toSeq).toSeq)

  /** Return the low points on grid as (rowNum, colNum) tuples */
  def lowPoints(grid: InputT): Iterable[Coord] =
    val isLowPoint = (coord: Coord) => {
      val elem = grid(coord)
      grid.axisNeighbors(coord).forall(c => grid(c) > elem)
    }

    for {
      coord <- grid.coords
      if isLowPoint(coord)
    } yield coord

  def solvePart1(grid: InputT): Int =
    lowPoints(grid)
      .map(c => grid(c) + 1)
      .sum

  /** Find the size of the basin containing the given location */
  def basinSize(grid: InputT, startCoord: Coord): Int =
    var toVisit = Vector(startCoord)
    var visited = Set.empty[Coord]

    while !toVisit.isEmpty do
      val coord = toVisit.head
      val neighbors = (grid.axisNeighbors(coord).toSet &~ visited).filter(c => grid(c) != 9)
      toVisit = toVisit.tail ++ neighbors
      visited += coord

    visited.size

  def solvePart2(grid: InputT): Int =
  // The problem description says that 9s don't belong to any basin, and all other locations belong to exactly one
  // basin, and that every basin has a low point.  Together, this means we can count on basins being surrounded by
  // 9s, and by traversing outward from each low point, we can find the size of each basin.

  // treat grid as an undirected graph.  Locations are nodes, and an edge exists between adjacent locations if neither
  // is a 9.  Then, basins are weakly connected components, and the problem becomes to find the 3 largest components.
  // We can cheat when finding the components by taking advantage of the fact that we know each low point is in a
  // distinct component, so we only need to find the size of each component.

    lowPoints(grid)
      .map { c => basinSize(grid, c) }
      .toSeq.sorted(Ordering.Int.reverse)
      .slice(0, 3)
      .product

  // alternate implementation using a graph library
  object Day9Graph extends AOCDay {

    import scalax.collection.Graph
    import scalax.collection.GraphPredef.EdgeAssoc
    import scalax.collection.GraphEdge.UnDiEdge

    // Parse input into a graph
    // Each node is a (value, (rowNum, colNum)) tuple
    override type InputT = Graph[(Int, (Int, Int)), UnDiEdge]

    override def parseInput(input: String): InputT =
      val grid = input.linesIterator.map(line => line.split("").map(_.toInt)).toSeq

      // Make a graph, with an edge between any two adjacent locations that are both not a 9
      // only need to look forward and down
      val neighborOffsets = Seq((0, 1), (1, 0))
      val edges = for {
        (row, rowNum) <- grid.zipWithIndex
        (elem, colNum) <- row.zipWithIndex
        if elem != 9
        (nr, nc) <- neighborOffsets.map { case (dr, dc) => (rowNum + dr, colNum + dc) }
        if grid.isDefinedAt(nr) && grid(nr).isDefinedAt(nc)
        nElem = grid(nr)(nc)
        if nElem != 9
      } yield (elem, (rowNum, colNum)) ~ (nElem, (nr, nc))

      Graph.from(edges = edges)

    def solvePart1(graph: InputT): Int =
      // for each connected component, the minimum value is the low point
      val lowPointVals = graph.componentTraverser().map(_.nodes.map(_._1).min)
      lowPointVals.map(_ + 1).sum

    def solvePart2(graph: InputT): Int =
      // Get the sizes of each connected component
      val basinSizes = graph.componentTraverser().map(_.nodes.size).toSeq
      basinSizes.sorted(Ordering.Int.reverse).slice(0, 3).product

    def main(args: Array[String]): Unit =
      val graph = parseInputFile("day9.txt")

      println(s"Solution for Part 1 = ${solvePart1(graph)}")
      println(s"Solution for Part 2 = ${solvePart2(graph)}")
  }


  def main(args: Array[String]): Unit =
    val grid = parseInputFile("day9.txt")

    println(s"Solution for Part 1 = ${solvePart1(grid)}")
    println(s"Solution for Part 2 = ${solvePart2(grid)}")

    println("Graph solution")
    Day9Graph.main(args)
}
