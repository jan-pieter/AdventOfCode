import scala.io.Source

object Problem13 extends App:
  sealed trait PacketData
  case class PacketList(values: Vector[PacketData]) extends PacketData
  case class PacketInt(value: Int) extends PacketData

  val IntPattern = "^([0-9]+)$".r
  val ListPattern = "^\\[(.*)\\]$".r

  def splitInside(str: String): Vector[String] = {
    val result = str.foldLeft[(Int, Vector[String])]((0, Vector(""))) {
      case ((0, parts), ',') => (0, parts :+ "")
      case ((level, parts), '[') => (level + 1, parts.updated(parts.length-1, parts(parts.length-1) :+ '['))
      case ((level, parts), ']') => (level - 1, parts.updated(parts.length-1, parts(parts.length-1) :+ ']'))
      case ((level, parts), char) => (level, parts.updated(parts.length-1, parts(parts.length-1) :+ char))
    }
    result._2
  }

  def parsePacketData(str: String): PacketData = str match {
    case IntPattern(value)   => PacketInt(value.toInt)
    case ListPattern(inside) => PacketList(splitInside(inside).filter(_.nonEmpty).map(parsePacketData))
    case other => sys.error(s"Unexpected: $other")
  }

  val input: Vector[(PacketData, PacketData)] = Source.fromResource("13-input.txt").getLines().filter(_.nonEmpty).grouped(2).map {
    case Seq(first, second) => parsePacketData(first) -> parsePacketData(second)
  }.toVector
//  input.foreach(println(_))

  def inOrder(pair: (PacketData, PacketData)): Option[Boolean] = {
//    println(s"InOrder: $pair")
    pair match {
      case (PacketInt(a), PacketInt(b)) if a == b => None
      case (PacketInt(a), PacketInt(b)) => Some(a < b)
      case (PacketList(a), PacketList(b)) => a.zip(b).foldLeft[Option[Boolean]](None){
        case (None, (x, y)) => inOrder(x, y)
        case (result, _) => result
      }.orElse(if (a.length == b.length) None else Some(a.length < b.length))
      case (a: PacketList, b: PacketInt) => inOrder(a, PacketList(Vector(b)))
      case (a: PacketInt, b: PacketList) => inOrder(PacketList(Vector(a)), b)
    }
  }

  val solution1 = input.zipWithIndex.filter((pair, _) => inOrder(pair).get).map(_._2 + 1)
  println(solution1.sum)

  val divider1 = PacketList(Vector(PacketList(Vector(PacketInt(2)))))
  val divider2 = PacketList(Vector(PacketList(Vector(PacketInt(6)))))
  val sorted = (Vector(divider1, divider2) ++ input.flatMap((a, b) => Vector(a, b))).foldLeft(Vector.empty[PacketData]){
    case (list, data) if list.isEmpty => Vector(data)
    case (list, data) => (list.takeWhile(a => inOrder(a, data).get) :+ data) ++ list.dropWhile(a => inOrder(a, data).get)
  }

//  sorted.foreach(println(_))

  val solution2 = (sorted.indexOf(divider1) + 1) * (sorted.indexOf(divider2) + 1)
  println(solution2)
