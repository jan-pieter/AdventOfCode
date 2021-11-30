import scala.collection.mutable
import scala.io.Source

object Problem14 extends App {

  val input = Source.fromResource("14-input.txt").getLines().toList

  sealed trait Instruction
  case class Mask(str: String)
  case class Mem(pos: Long, value: Long)

  val MaskPattern = "^mask = (.*)$".r
  val MemPattern = "^mem\\[(\\d+)\\] = (\\d+)$".r

  val instructions = input.map {
    case MaskPattern(str) => Mask(str)
    case MemPattern(pos, value) => Mem(pos.toLong, value.toLong)
  }

  def to36Bit(value: Long): String = value.toBinaryString.reverse.padTo(36, '0').reverse
  def from36Bit(str: String): Long = BigInt(str, 2).longValue
  def applyMask(str: String, mask: String): String = {
    mask.zip(str).map {
      case ('X', i) => i
      case (i, _) => i
    }.mkString("")
  }
  def applyMask2(str: String, mask: String): List[String] = {
    mask.zip(str).map {
      case ('X', _) => List('0', '1')
      case ('1', _) => List('1')
      case ('0', i) => List(i)
    }.foldLeft(List("")) {
      case (strings, chars) if chars.size == 1 => strings.map(str => str + chars.head)
      case (strings, _) => strings.flatMap(str => List(str + '0', str + '1'))
    }
  }

  var currentMask = ""
  val memory: mutable.Map[Long, Long] = mutable.Map.empty

  instructions.foreach {
    case Mask(str) => currentMask = str
    case Mem(pos, value) => memory.put(pos, from36Bit(applyMask(to36Bit(value), currentMask)))
  }

  println(memory.values.sum)

  currentMask = ""
  memory.clear()
  println(memory.values.sum)

//  println(applyMask2("000000000000000000000000000000011010", "00000000000000000000000000000000X0XX"))
//  println(instructions)

  instructions.foreach {
    case Mask(str) => currentMask = str
    case Mem(pos, value) => applyMask2(to36Bit(pos), currentMask).foreach(address => memory.put(from36Bit(address), value))
  }

  println(memory.values.sum)

}
