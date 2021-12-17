package aoc2021

// Solution to https://adventofcode.com/2021/day/10

object Day10 extends AOCDay {
  override type InputT = Seq[String]

  type Pos = Int

  enum SyntaxCheckResult:
    case Completed(added: String)
    case Error(pos: Pos, char: Char)
    case Ok(pos: Pos)

  val closeDelims = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>'
  )

  override def parseInput(input: String): Seq[String] =
    input.linesIterator.toSeq

  def checkSyntax(line: String): SyntaxCheckResult =
    import SyntaxCheckResult._

    def checkSyntaxInner(line: String, pos: Pos): SyntaxCheckResult =
      if pos >= line.length then
        Ok(pos)
      else
        closeDelims.get(line(pos)) match {
          case Some(closeDelim) => checkSyntaxInner(line, pos + 1) match {
            case Ok(newPos) if newPos >= line.length => Completed(closeDelim.toString)
            case Completed(added) => Completed(added + closeDelim)
            case Ok(newPos) if closeDelim == line(newPos) => checkSyntaxInner(line, newPos + 1)
            case Ok(newPos) => Error(newPos, line(newPos))
            case other => other
          }
          case None => Ok(pos)
        }

    checkSyntaxInner(line, 0) match {
      case Ok(pos) if pos < line.length => Error(pos, line(pos))
      case other => other
    }

  def solvePart1(input: InputT): Int =
    val delimScores = Map(
      ')' -> 3,
      ']' -> 57,
      '}' -> 1197,
      '>' -> 25137
    )

    input.iterator.map(checkSyntax).collect {
      case SyntaxCheckResult.Error(_, ch) => delimScores(ch)
    }.sum

  // need to return Long to avoid overflow
  def solvePart2(input: InputT): Long =
    val delimScores = Map(
      ')' -> 1,
      ']' -> 2,
      '}' -> 3,
      '>' -> 4
    )

    val scores = input.map(checkSyntax).collect {
      case SyntaxCheckResult.Completed(added) => added.foldLeft(0L) { (score, ch) =>
        score * 5 + delimScores(ch)
      }
    }.sorted

    scores(scores.length / 2)

  def main(args: Array[String]): Unit = {
    val input = parseInputFile("day10.txt")

    println(s"Solution to part 1 = ${solvePart1(input)}")
    println(s"Solution to part 2 = ${solvePart2(input)}")
  }
}
