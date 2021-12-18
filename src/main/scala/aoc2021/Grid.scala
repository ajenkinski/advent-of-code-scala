package aoc2021

type Grid[A] = Vector[Vector[A]]

extension [A](g: Grid[A])
  def numRows: Int = g.length
  def numCols: Int = g(0).length

  def coords: Iterable[Coord] =
    for r <- g.indices.view; c <- g(0).indices.view yield Coord(r, c)

  def isDefinedAt(c: Coord): Boolean = g.isDefinedAt(c.row) && g(c.row).isDefinedAt(c.col)

  def axisNeighbors(c: Coord): Seq[Coord] = Coord.axisOffsets.map(_ + c).filter(isDefinedAt(_))
  def allNeighbors(c: Coord): Seq[Coord] = Coord.allOffsets.map(_ + c).filter(isDefinedAt(_))

  def apply(coord: Coord): A = g(coord.row)(coord.col)

  def updated(c: Coord, newVal: A): Grid[A] =
    g.updated(c.row, g(c.row).updated(c.col, newVal))

  def mapGrid[B](f: A => B): Grid[B] = g.map(_.map(f))

object Grid:
  def apply[A](rows: Int, cols: Int, fillVal: A): Grid[A] = Vector.fill(rows, cols)(fillVal)
  def from[A](source: Iterable[Iterable[A]]): Grid[A] = Vector.from(source.map(_.toVector))
