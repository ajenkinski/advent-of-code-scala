package aoc2021

// Solution to https://adventofcode.com/2021/day/16

import scala.util.parsing.combinator.*

object Day16 extends AOCDay {
  override type InputT = String

  override def parseInput(input: String): String =
    input.flatMap { hexDigit =>
      val s = Integer.parseInt(hexDigit.toString, 16).toBinaryString
      if s.length == 4 then s else ("0" * (4 - s.length)) + s
    }

  trait Packet {
    val version: Int
    val typeId: Int
  }

  case class Literal(version: Int, typeId: Int, value: Long) extends Packet
  case class Operator(version: Int, typeId: Int, children: Seq[Packet]) extends Packet

  // I use a parser combinator library to create a packet parser
  class PacketParser extends RegexParsers:
    def version: Parser[Int] = raw"\d{3}".r ^^ { Integer.parseInt(_, 2) }
    def typeId: Parser[Int] = raw"\d{3}".r ^^ { Integer.parseInt(_, 2) }

    // Bits for one digit other than the last digit of a literal
    def literalGroup: Parser[String] = raw"1\d{4}".r ^^ { _.tail.toString }
    // Bits for the last digit of a literal
    def literalEndGroup: Parser[String] = raw"0\d{4}".r ^^ { _.tail.toString }
    def literalValue: Parser[Long] = rep(literalGroup) ~ literalEndGroup ^^ {
      case parts ~ lastPart => java.lang.Long.parseLong(parts.mkString + lastPart, 2)
    }
    def literalPacket: Parser[Packet] = version ~ (typeId ^? { case n if n == 4 => n }) ~ literalValue ^^ {
      case v ~ t ~ value => Literal(v, t, value)
    }

    // number of bits in an operator packet
    def numBits: Parser[Int] = raw"\d{15}".r ^^ { Integer.parseInt(_, 2) }
    // number of packets in an operator packet
    def numPackets: Parser[Int] = raw"\d{11}".r ^^ { Integer.parseInt(_, 2) }

    def operatorPacket: Parser[Packet] = version ~ (typeId ^? { case n if n != 4 => n }) ~ operatorPacketPayload ^^ {
      case v ~ t ~ children => Operator(v, t, children)
    }

    // payload where the number of packets is specified
    def numPacketsPayload: Parser[Seq[Packet]] = "1" ~> numPackets >> { n => repN(n, packet )}

    // payload where number of bits is specified
    def numBitsPayload: Parser[Seq[Packet]] = "0" ~> numBits >> { n => repN(n, ".".r) } ^^ { bits =>
      parse(rep1(packet), bits.mkString) match {
        case Success(packets, _) => packets
        case e => throw new Exception(s"Got error while parsing bits payload: $e")
      }
    }

    def operatorPacketPayload: Parser[Seq[Packet]] = numPacketsPayload | numBitsPayload

    def packet: Parser[Packet] = literalPacket | operatorPacket

  private def parsePacket(bits: String): Packet =
    val parser = PacketParser()

    parser.parse(parser.packet, bits) match {
      case parser.Success(packet, _) => packet
      case e => throw new Exception(s"Parsing failed with $e")
    }

  def foldPackets[A](packet: Packet, accum: A)(f: (A, Packet) => A): A =
    val newAccum = f(accum, packet)
    packet match {
      case Literal(_, _, _) => newAccum
      case Operator(_, _, children) => children.foldLeft(newAccum)((a, packet) => foldPackets(packet, a)(f))
    }

  def solvePart1(bits: String): Long =
    val topPacket = parsePacket(bits)
    foldPackets(topPacket, 0L) { (sum, packet) => sum + packet.version }

  def evaluatePacket(packet: Packet): Long =
    packet match {
      case Literal(_, _, value) => value
      case Operator(_, typeId, packets) =>
        val values = packets.map(evaluatePacket)
        typeId match {
          case 0 => values.sum
          case 1 => values.product
          case 2 => values.min
          case 3 => values.max
          case 5 => if values(0) > values(1) then 1 else 0
          case 6 => if values(0) < values(1) then 1 else 0
          case 7 => if values(0) == values(1) then 1 else 0
          case _ => throw new Exception(s"Unexpected typeId: $typeId")
        }
    }

  def solvePart2(bits: String): Long =
    val topPacket = parsePacket(bits)
    evaluatePacket(topPacket)

  def main(args: Array[String]): Unit =
    val input = parseInputFile("day16.txt")

    println(s"Solution to part 1 = ${solvePart1(input)}")
    println(s"Solution to part 2 = ${solvePart2(input)}")

}
