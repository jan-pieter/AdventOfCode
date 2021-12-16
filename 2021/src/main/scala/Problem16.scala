import scala.collection.mutable
import scala.io.Source

object Problem16 extends App {

  val input: String = Source.fromResource("16-input.txt").getLines().toVector.head
//  val input = "D2FE28"
//  val input = "38006F45291200"
//  val input = "EE00D40C823060"
//  val input = "9C0141080250320F1802104A08"

  val bits: String = input.flatMap(c => Integer.parseInt(c.toString, 16).toBinaryString.reverse.padTo(4, '0').reverse)

  println(s"Bits $bits")

  trait Packet {
    def version: Int
  }
  case class Literal(version: Int, value: Long) extends Packet
  case class Operator(version: Int, typeId: Int, subPackets: Vector[Packet]) extends Packet

  def dequeue(queue: mutable.Queue[Char], n: Int): String = {
    (0 until n).map(_ => queue.dequeue()).mkString
  }

  def parsePacket(string: String): (Packet, String) = {
    val queue = mutable.Queue.from(string.toVector)
    val version = Integer.parseInt(dequeue(queue, 3), 2)
    val packetType = Integer.parseInt(dequeue(queue, 3), 2)
    println(s"Version $version Type $packetType")
    packetType match {
      case 4 =>
        var done = false
        var value = ""
        while (!done) {
          val part = dequeue(queue, 5)
          val v = part.drop(1)
          value += v
          done = part.head == '0'
        }
        Literal(version, java.lang.Long.parseLong(value, 2)) -> queue.mkString
      case typeId =>
        val lengthType = queue.dequeue()
        lengthType match {
          case '0' =>
            val bits = Integer.parseInt(dequeue(queue, 15), 2)
            var toParse = dequeue(queue, bits)
            var packets: List[Packet] = List.empty
            while (!toParse.forall(_ == '0')) {
              val (packet, rest) = parsePacket(toParse)
              packets = packet :: packets
              toParse = rest
            }
            Operator(version, typeId, packets.reverse.toVector) -> queue.mkString
          case '1' =>
            val subPackets = Integer.parseInt(dequeue(queue, 11), 2)
            var packets: List[Packet] = List.empty
            var toParse = queue.mkString
            while(packets.size < subPackets) {
              val (packet, rest) = parsePacket(toParse)
              packets = packet :: packets
              toParse = rest
            }
            Operator(version, typeId, packets.reverse.toVector) -> toParse
        }
    }
  }

  val result = parsePacket(bits)
  println(result)

  def totalVersion(packet: Packet): Int = packet match {
    case Literal(version, _) => version
    case Operator(version, _, subPackets) => version + subPackets.map(totalVersion).sum
  }

  println(s"Solution1: ${totalVersion(result._1)}")

  def totalValue(packet: Packet): Long = packet match {
    case Literal(version, value) => value
    case Operator(version, 0, subPackets) => subPackets.map(totalValue).sum
    case Operator(version, 1, subPackets) => subPackets.map(totalValue).product
    case Operator(version, 2, subPackets) => subPackets.map(totalValue).min
    case Operator(version, 3, subPackets) => subPackets.map(totalValue).max
    case Operator(version, 5, Vector(a, b)) => if (totalValue(a) > totalValue(b)) 1L else 0L
    case Operator(version, 6, Vector(a, b)) => if (totalValue(a) < totalValue(b)) 1L else 0L
    case Operator(version, 7, Vector(a, b)) => if (totalValue(a) == totalValue(b)) 1L else 0L
  }

  println(s"Solution2: ${totalValue(result._1)}")




}
