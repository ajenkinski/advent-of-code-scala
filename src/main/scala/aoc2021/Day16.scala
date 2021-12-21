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
  }

  case class Literal(version: Int, value: Long) extends Packet
  case class Operator(version: Int, typeId: Int, children: Seq[Packet]) extends Packet

  // I use a parser combinator library to create a packet parser
  class PacketParser extends RegexParsers:
    // create a parser that parsers an int from the given number of bits
    def int(nbits: Int): Parser[Int] = repN(nbits, ".".r) ^^ {
      bits => Integer.parseInt(bits.mkString, 2)
    }

    // create a parser that succeeds if p succeeds and the given predicate on the result of p returns true
    def acceptIf[T](p: Parser[T], pred: T => Boolean): Parser[T] =
      p ^? { case x if pred(x) => x }

    def version: Parser[Int] = int(3)
    def typeId: Parser[Int] = int(3)

    // Bits for one digit other than the last digit of a literal
    def literalGroup: Parser[String] = raw"1\d{4}".r ^^ { _.tail.toString }
    // Bits for the last digit of a literal
    def literalEndGroup: Parser[String] = raw"0\d{4}".r ^^ { _.tail.toString }
    def literalValue: Parser[Long] = rep(literalGroup) ~ literalEndGroup ^^ {
      case parts ~ lastPart => java.lang.Long.parseLong(parts.mkString + lastPart, 2)
    }
    def literalPacket: Parser[Packet] = version ~ acceptIf(typeId, _ == 4) ~ literalValue ^^ {
      case v ~ t ~ value => Literal(v, value)
    }

    def operatorPacket: Parser[Packet] = version ~ acceptIf(typeId, _ != 4) ~ operatorPacketPayload ^^ {
      case v ~ t ~ children => Operator(v, t, children)
    }

    // payload where the number of packets is specified
    def numPacketsPayload: Parser[Seq[Packet]] = "1" ~> int(11) >> { repN(_, packet ) }

    // payload where number of bits is specified
    def numBitsPayload: Parser[Seq[Packet]] = "0" ~> int(15) >> { repN(_, ".".r) } ^^ { bits =>
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
      case Literal(_, _) => newAccum
      case Operator(_, _, children) => children.foldLeft(newAccum)((a, packet) => foldPackets(packet, a)(f))
    }

  def solvePart1(bits: String): Long =
    val topPacket = parsePacket(bits)
    foldPackets(topPacket, 0L) { (sum, packet) => sum + packet.version }

  def evaluatePacket(packet: Packet): Long =
    packet match {
      case Literal(_, value) => value
      case Operator(_, typeId, packets) =>
        val args = packets.map(evaluatePacket)
        typeId match {
          case 0 => args.sum
          case 1 => args.product
          case 2 => args.min
          case 3 => args.max
          case 5 => if args(0) > args(1) then 1 else 0
          case 6 => if args(0) < args(1) then 1 else 0
          case 7 => if args(0) == args(1) then 1 else 0
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
