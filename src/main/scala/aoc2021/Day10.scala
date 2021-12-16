package aoc2021

object Day10 extends AOCDay {
  override type InputT = Seq[String]

  type Pos = Int

  enum SyntaxCheckResult:
    case Incomplete
    case Error(pos: Pos, char: Char)
    case Ok(pos: Pos)

  val delims = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>'
  )

  override def parseInput(input: String): Seq[String] =
    input.linesIterator.toSeq

  def checkSyntax(line: String): SyntaxCheckResult =
    def checkSyntaxInner(line: String, pos: Pos): SyntaxCheckResult =
      import SyntaxCheckResult._
      if pos >= line.length then
        Ok(pos)
      else
        val ch = line(pos)
        if delims.contains(ch) then
          checkSyntaxInner(line, pos + 1) match {
            case Ok(newPos) if newPos >= line.length => Incomplete
            case Ok(newPos) =>
              if delims(ch) == line(newPos) then
                checkSyntaxInner(line, newPos + 1)
              else
                Error(newPos, line(newPos))
            case other => other
          }
        else
          Ok(pos)

    checkSyntaxInner(line, 0) match {
      case SyntaxCheckResult.Ok(pos) if pos < line.length => SyntaxCheckResult.Error(pos, line(pos))
      case other => other
    }

  def solvePart1(input: InputT): Int =
    val delimScores = Map(
      ')' -> 3,
      ']' -> 57,
      '}' -> 1197,
      '>' -> 25137
    )

    input.iterator.map { line =>
      checkSyntax(line) match {
        case SyntaxCheckResult.Error(_, ch) => delimScores(ch)
        case _ => 0
      }
    }.sum

  def main(args: Array[String]): Unit = {
    val input = parseInputFile("day10.txt")

    println(s"Solution to part 1 = ${solvePart1(input)}")
  }
}
