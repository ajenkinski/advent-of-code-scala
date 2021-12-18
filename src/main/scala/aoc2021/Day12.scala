package aoc2021

// Solution to https://adventofcode.com/2021/day/12

import scalax.collection.Graph
import scalax.collection.GraphPredef.EdgeAssoc
import scalax.collection.GraphEdge.UnDiEdge

object Day12 extends AOCDay {
  override type InputT = Graph[String, UnDiEdge]

  override def parseInput(input: String): InputT =
    val edges = input.linesIterator.map(_.split("-")).collect {
      case Array(a, b) => a ~ b
    }.toSeq

    Graph.from(edges = edges)

  def findAllPaths(graph: InputT, canRepeat: Boolean, startLabel: String = "start",
                   endLabel: String = "end"): Seq[List[graph.NodeT]] =
    def inner(node: graph.NodeT, seen: Set[graph.NodeT], canRepeat: Boolean): Seq[List[graph.NodeT]] =
      if node.toOuter == endLabel then
        Seq(List(node))
      else
        val neighbors = node.neighbors.map(_.asInstanceOf[graph.NodeT])
        val newSeen = if node.charAt(0).isLower then seen + node else seen
        neighbors.toSeq.flatMap { nextNode =>
          if seen.contains(nextNode) then
            if canRepeat && nextNode.toOuter != startLabel then
              inner(nextNode, newSeen, false).map(node :: _)
            else
              Seq.empty
          else
            inner(nextNode, newSeen, canRepeat).map(node :: _)
        }

    inner(graph.get(startLabel), Set.empty, canRepeat)

  def solvePart1(graph: InputT): Int =
    findAllPaths(graph, false).length

  def solvePart2(graph: InputT): Int =
    findAllPaths(graph, true).length

  def main(args: Array[String]): Unit =
    val input = parseInputFile("day12.txt")

    println(s"Solution to part 1 = ${solvePart1(input)}")
    println(s"Solution to part 2 = ${solvePart2(input)}")
}
