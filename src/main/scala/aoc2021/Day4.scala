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
        if cell.num == num && !cell.filled then
          val newBoard = board.updated(rowNum, row.updated(colNum, cell.copy(filled = true)))
          // check if there's a bingo
          val bingo = newBoard(rowNum).forall(_.filled) || newBoard.forall(_(colNum).filled)
          return (newBoard, bingo)

    (board, false)

  /**
   * Solver for part 1 and 2
   * @param problem
   * @param findFirst If true, return answer for first solved board, else return answer for last solved board
   * @return Score for the first or last solved board
   */
  def solve(problem: Problem, findFirst: Boolean): Int =
    var boards = problem.boards
    var lastSolution: Option[Int] = None

    for num <- problem.numbers do
      var newBoards = Vector.empty[Board]

      for (board, boardNum) <- boards.zipWithIndex do
        val (newBoard, bingo) = fillNumber(board, num)
        if bingo then
          val unfilledSum = (for row <- newBoard; cell <- row if !cell.filled yield cell.num).sum
          val solution = unfilledSum * num
          if findFirst then
            return solution
          else
            lastSolution = Some(solution)
        else
          // keep updated boards but filter out bingoed boards
          newBoards = newBoards :+ newBoard

      boards = newBoards

    lastSolution.get

  def solvePart1(problem: Problem): Int =
    solve(problem, true)

  def solvePart2(problem: Problem): Int =
    solve(problem, false)

  def main(args: Array[String]): Unit =
    val problem = parseInput(Utils.readInput("day4.txt"))

    val part1Solution = solvePart1(problem)
    println(s"Part 1 solution = $part1Solution")

    val part2Solution = solvePart2(problem)
    println(s"Part 2 solution = $part2Solution")
}
