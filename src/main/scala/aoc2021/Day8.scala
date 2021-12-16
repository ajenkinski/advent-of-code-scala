package aoc2021

// Solution to https://adventofcode.com/2021/day/8

object Day8 extends AOCDay {
  override type InputT = Seq[Entry]
  
  case class Entry(patterns: Seq[String], digits: Seq[String])

  /** A map from wire to possible segments it could represent */
  type SegmentMap = Map[Char, Set[Char]]

  /** Map digit to segments in digit */
  val digitSegments = Vector(
    "abcefg", // 0
    "cf", // 1
    "acdeg", // 2
    "acdfg", // 3
    "bcdf", // 4
    "abdfg", // 5
    "abdefg", // 6
    "acf", // 7
    "abcdefg", // 8
    "abcdfg" // 9
  ).map(_.toSet)

  /** digits that have a unique number of segments */
  val uniqueDigits = Vector(1, 4, 7, 8)

  /** segment counts of unique digits */
  val uniqueSegmentCounts = Vector(2, 4, 3, 7)

  override def parseInput(input: String): InputT =
    input.linesIterator.map { line =>
      line.split(raw"\|") match {
        case Array(pats, digits) => Entry(pats.trim.split(" "), digits.trim.split(" "))
        case _ => throw new Exception(s"Couldn't parse line: '$line''")
      }
    }.toSeq

  def solvePart1(entries: Seq[Entry]): Int =
    entries
      .map(entry => entry.digits.map(_.length).count(uniqueSegmentCounts.contains(_)))
      .sum

  def updateSegmentMap(segmentMap: SegmentMap, wires: Seq[Char], segments: Set[Char]): SegmentMap =
    segmentMap.map { (ch, segs) =>
      val newSegs = if wires.contains(ch) then
        segs & segments
      else
        segs &~ segments

      (ch, newSegs)
    }

  // Return all possible sets of segments the given pattern could form, based on the given segment map
  def getPossibleDigits(segmentMap: SegmentMap, pattern: String): Seq[Set[Char]] =
    if pattern.isEmpty then
      Seq(Set())
    else
      val rests = getPossibleDigits(segmentMap, pattern.tail)
      segmentMap(pattern(0))
        .flatMap { segment =>
          rests.map(s => s ++ Set(segment))
        }
        .filter(_.size == pattern.length)
        .toSeq

  /** Given an Entry, returns a map from patterns to the digit it represents */
  def decodeEntry(entry: Entry): Map[Set[Char], Int] =
    val allSegments = "abcdefg".toSet

    // Map from wire to possible segments it could map to. Initially each wire could map to any segment
    var segmentMap = allSegments.map((_, allSegments)).toMap

    // Use unique count digits to narrow down segment mappings
    segmentMap = entry.patterns
      .foldLeft(segmentMap) { (mapping, pattern) =>
        val len = pattern.length
        if uniqueSegmentCounts.contains(len) then
          // we know which digit pattern represents
          val digit = uniqueDigits(uniqueSegmentCounts.indexOf(len))
          updateSegmentMap(mapping, pattern, digitSegments(digit))
        else
          mapping
      }

    // map known patterns to digits
    val patternMap = entry.patterns
      .filter(p => uniqueSegmentCounts.contains(p.length))
      .map(p => (p.toSet, uniqueDigits(uniqueSegmentCounts.indexOf(p.length))))
      .toMap

    // deduce the rest of the patternMap using possible digits
    val unknowPatterns = entry.patterns.map(_.toSet).toSet &~ patternMap.keys.toSet
    unknowPatterns.foldLeft(patternMap) { (pmap, pattern) =>
      val possibleDigits = getPossibleDigits(segmentMap, pattern.mkString).filter(digitSegments.contains(_))
      if possibleDigits.length == 1 then
        pmap.updated(pattern, digitSegments.indexOf(possibleDigits.head))
      else
        pmap
    }

  def solvePart2(entries: Seq[Entry]): Int =
    entries
      .map { entry =>
        val decoded = decodeEntry(entry)
        val decodedDigits = entry.digits.map(d => decoded(d.toSet))
        decodedDigits.mkString.toInt
      }
      .sum

  def main(args: Array[String]): Unit = {
    val entries = parseInputFile("day8.txt")

    println(s"Solution for Part 1 = ${solvePart1(entries)}")
    println(s"Solution for Part 2 = ${solvePart2(entries)}")
  }
}
