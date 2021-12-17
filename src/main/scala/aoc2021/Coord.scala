package aoc2021

case class Coord(row: Int, col: Int):
  def +(other: Coord): Coord = Coord(row + other.row, col + other.col)

object Coord:
  val axisOffsets: Seq[Coord] = Seq(Coord(0, 1), Coord(-1, 0), Coord(1, 0), Coord(0, -1))
  val diagonalOffsets: Seq[Coord] = Seq(Coord(-1, 1), Coord(1, 1), Coord(-1, -1), Coord(1, -1))
  val allOffsets: Seq[Coord] = axisOffsets ++ diagonalOffsets
  