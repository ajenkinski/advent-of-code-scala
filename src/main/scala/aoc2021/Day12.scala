package aoc2021

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

  def findAllPaths(graph: InputT, startLabel: String = "start", endLabel: String = "end"): Seq[List[graph.NodeT]] =
    def inner(node: graph.NodeT, seen: Set[graph.NodeT]): Seq[List[graph.NodeT]] =
      if node.toOuter == endLabel then
        Seq(List(node))
      else
        val nextNodes = node.neighbors.map(_.asInstanceOf[graph.NodeT]) &~ seen
        if nextNodes.isEmpty then
          Seq.empty
        else
          val newSeen = if node.charAt(0).isLower then seen + node else seen
          nextNodes.toSeq.flatMap(nextNode => inner(nextNode, newSeen).map(node :: _))

    inner(graph.get(startLabel), Set.empty)

  def solvePart1(graph: InputT): Int =
    findAllPaths(graph).length

  def main(args: Array[String]): Unit =
    val input = parseInputFile("day12.txt")

    println(s"Solution to part 1 = ${solvePart1(input)}")
}
