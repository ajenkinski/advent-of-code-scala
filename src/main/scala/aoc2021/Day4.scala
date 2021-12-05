package aoc2021

// Solution to https://adventofcode.com/2021/day/4

object Day4 {
  case class BoardCell(num: Int, filled: Boolean)

  type Board = Seq[Seq[BoardCell]]

  case class Problem(numbers: Seq[Int], boards: Seq[Board])

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
        if cell.num == num && !cell.filled then
          val newBoard = board.updated(rowNum, row.updated(colNum, cell.copy(filled = true)))
          // check if there's a bingo
          val bingo = newBoard(rowNum).forall(_.filled) || newBoard.forall(_(colNum).filled)
          return (newBoard, bingo)

    (board, false)

  /**
   * Runs through all the numbers in the problem, and returns the scores of the solved boards in the
   * order that they were solved.
   */
  def playAllNums(problem: Problem): Seq[Int] =
    val (_, scores) = problem.numbers.foldLeft((problem.boards, Seq.empty[Int])) {
      case ((unsolved, scores), num) =>
        unsolved.foldLeft((Seq.empty[Board], scores)) {
          case ((newUnsolved, newScores), board) =>
            val (newBoard, bingo) = fillNumber(board, num)
            if bingo then
              val unfilledSum = (for row <- newBoard; cell <- row if !cell.filled yield cell.num).sum
              val score = unfilledSum * num
              (newUnsolved, newScores :+ score)
            else
              (newUnsolved :+ newBoard, newScores)
        }
    }

    scores

  def main(args: Array[String]): Unit =
    val problem = parseInput(Utils.readInput("day4.txt"))
    val scores = playAllNums(problem)

    println(s"Part 1 solution = ${scores.head}")
    println(s"Part 2 solution = ${scores.last}")
}
