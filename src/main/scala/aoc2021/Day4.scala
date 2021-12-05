package aoc2021


object Day4 {
  case class BoardCell(num: Int, filled: Boolean)

  type Board = Seq[Seq[BoardCell]]

  case class Problem(val numbers: Seq[Int], val boards: Seq[Board])

  def parseInput(input: String): Problem =
    // split on blank lines
    val parts = input.split("\n\\s*\n")

    // first line is the numbers to draw
    val numbers = parts(0).split(",").map(_.toInt).toSeq

    val boards = parts.tail.map { boardStr =>
      boardStr
        .linesIterator
        .map(_.trim.split(" +").map(s => BoardCell(s.toInt, false)).toVector)
        .toVector
    }.toVector

    Problem(numbers, boards)

  /**
   * Marks number on a bingo board if board contains the number, and checks whether the new mark
   * caused a bingo.
   *
   * @param board
   * @param num The number to search for
   * @return A new board with number filled in if board contained number, and a boolean indicating whether the
   *         the new fill created a bingo
   */
  def fillNumber(board: Board, num: Int): (Board, Boolean) =
    for (row, rowNum) <- board.zipWithIndex do
      for (cell, colNum) <- row.zipWithIndex do
        if cell.num == num then
          val newBoard = board.updated(rowNum, row.updated(colNum, cell.copy(filled = true)))
          // check if there's a bingo
          val bingo = newBoard(rowNum).forall(_.filled) || newBoard.forall(_(colNum).filled)
          return (newBoard, bingo)

    (board, false)

  def solvePart1(problem: Problem): Int =
    var boards = problem.boards

    for num <- problem.numbers do
      for (board, boardNum) <- boards.zipWithIndex do
        val (newBoard, bingo) = fillNumber(board, num)
        if bingo then
          val unfilledSum = (for row <- newBoard; cell <- row if !cell.filled yield cell.num).sum
          return unfilledSum * num
        else
          boards = boards.updated(boardNum, newBoard)

    throw new Exception("Couldn't solve part 1")

  def main(args: Array[String]): Unit =
    val problem = parseInput(Utils.readInput("day4.txt"))

    val part1Solution = solvePart1(problem)
    println(s"Part 1 solution = $part1Solution")
}
