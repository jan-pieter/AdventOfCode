import scala.io.Source

object Problem21 extends App:

  val monkeys: Map[String, Monkey] = Source.fromResource("21-input.txt").getLines().toVector.map {
    case s"$id: $ref1 + $ref2" => id -> Monkey(id, MonkeyOp(ref1, (r1, r2) => r1 + r2, ref2, (r1, r2) => r1 - r2, (r1, r2) => r1 - r2))
    case s"$id: $ref1 - $ref2" => id -> Monkey(id, MonkeyOp(ref1, (r1, r2) => r1 - r2, ref2, (r1, r2) => r1 + r2, (r1, r2) => r2 - r1))
    case s"$id: $ref1 * $ref2" => id -> Monkey(id, MonkeyOp(ref1, (r1, r2) => r1 * r2, ref2, (r1, r2) => r1 / r2, (r1, r2) => r1 / r2))
    case s"$id: $ref1 / $ref2" => id -> Monkey(id, MonkeyOp(ref1, (r1, r2) => r1 / r2, ref2, (r1, r2) => r1 * r2, (r1, r2) => r1 / r2))
    case s"$id: $value" => id -> Monkey(id, MonkeyVal(value.toLong))
  }.toMap

  sealed trait MonkeyNumber {
    def value: Long
  }
  case class MonkeyVal(value: Long) extends MonkeyNumber
  case class MonkeyOp(ref1: String, op: (Long, Long) => Long, ref2: String, invOpLeft: (Long, Long) => Long, invOpRight: (Long, Long) => Long) extends MonkeyNumber {
    lazy val value: Long = op(monkeys(ref1).number.value, monkeys(ref2).number.value)
  }

  case class Monkey(id: String, number: MonkeyNumber) {
    lazy val realValue: Long | (Long => Long) = (id, number) match {
      case ("humn", _) => identity
      case ("root", MonkeyOp(ref1, _, ref2, _, _)) => (monkeys(ref1).realValue, monkeys(ref2).realValue) match {
        case (number: Long, f: (Long => Long)) => f(number)
        case (f: (Long => Long), number: Long) => f(number)
      }
      case (_, MonkeyVal(value)) => value
      case (_, MonkeyOp(ref1, op, ref2, invOpLeft, invOpRight)) => (monkeys(ref1).realValue, monkeys(ref2).realValue) match {
        case (number: Long, number2: Long) => op(number, number2)
        case (number: Long, f: (Long => Long)) => x => f(invOpRight(x, number))
        case (f: (Long => Long), number: Long) => x => f(invOpLeft(x, number))
      }
    }
  }

  val solution1 = monkeys("root").number.value
  println(solution1)

  val solution2 = monkeys("root").realValue
  println(solution2)
