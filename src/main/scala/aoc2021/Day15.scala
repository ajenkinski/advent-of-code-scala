package aoc2021

// Solution to https://adventofcode.com/2021/day/15

import scalax.collection.Graph
import scalax.collection.GraphPredef.EdgeAssoc
import scalax.collection.edge.WDiEdge
import scalax.collection.edge.Implicits._

object Day15 extends AOCDay {
  override type InputT = Grid[Int]

  override def parseInput(input: String): InputT =
    Grid.from(input.linesIterator.map(_.split("").map(_.toInt).toSeq).toSeq)

  def toGraph(grid: Grid[Int]): Graph[Coord, WDiEdge] =
    val edges = grid.coords.flatMap { coord =>
      grid.axisNeighbors(coord).map(ncoord => ncoord ~> coord % grid(coord))
    }

    Graph.from(edges = edges)

  def solve(grid: InputT): Int =
    val start = Coord(0, 0)
    val dest = Coord(grid.numRows - 1, grid.numCols - 1)
    val graph = toGraph(grid)

    val Some(path) = graph.get(start).shortestPathTo(graph.get(dest))(identity)
    path.edges.map(_.weight.toInt).sum

  def solvePart1(grid: InputT): Int = solve(grid)

  def solvePart2(grid: InputT): Int =
    val bigGrid = Grid.from((0 until 5).flatMap { tileRowNum =>
      grid.map { row =>
        (0 until 5).flatMap(tileColNum => row.map(x => (x + tileRowNum + tileColNum - 1) % 9 + 1))
      }
    })

    solve(bigGrid)

  def main(args: Array[String]): Unit =
    val input = parseInputFile("day15.txt")

    println(s"Solution to part 1 = ${solvePart1(input)}")
    println(s"Solution to part 2 = ${solvePart2(input)}")

}
