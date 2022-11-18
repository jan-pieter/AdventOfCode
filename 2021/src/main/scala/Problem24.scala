import scala.collection.parallel.CollectionConverters._
import scala.io.Source

object Problem24 extends App {

  sealed trait Instruction
  case class Inp(target: String) extends Instruction
  case class Add(target: String, other: String) extends Instruction
  case class Mul(target: String, other: String) extends Instruction
  case class Div(target: String, other: String) extends Instruction
  case class Mod(target: String, other: String) extends Instruction
  case class Eql(target: String, other: String) extends Instruction

  def parseInstruction(line: String): Instruction = line match {
    case s"inp $target" => Inp(target)
    case s"add $target $other" => Add(target, other)
    case s"mul $target $other" => Mul(target, other)
    case s"div $target $other" => Div(target, other)
    case s"mod $target $other" => Mod(target, other)
    case s"eql $target $other" => Eql(target, other)
  }

  val program = Source.fromResource("24-input.txt").getLines().map(parseInstruction).toVector

  def run(input: String): Option[Boolean] = {
    val (left, state, success) = program.foldLeft((input, Map("w" -> 0L, "x" -> 0L, "y" -> 0L, "z" -> 0L), true)) {
      case ((left, state, false), _) => (left, state, false)
      case ((left, state, _), Inp(target)) => (left.tail, state + (target -> left.head.asDigit.toLong), true)
      case ((left, state, _), Add(target, other)) => (left, state + (target -> (state(target) + state.getOrElse(other, other.toInt.toLong))), true)
      case ((left, state, _), Mul(target, other)) => (left, state + (target -> (state(target) * state.getOrElse(other, other.toInt.toLong))), true)
      case ((left, state, _), Div(_, other)) if state.getOrElse(other, other.toInt.toLong) == 0 => (left, state, false)
      case ((left, state, _), Div(target, other)) => (left, state + (target -> (state(target) / state.getOrElse(other, other.toInt.toLong))), true)
      case ((left, state, _), Mod(target, other)) if state(target) < 0 || state.getOrElse(other, other.toInt.toLong) <= 0 => (left, state, false)
      case ((left, state, _), Mod(target, other)) => (left, state + (target -> (state(target) % state.getOrElse(other, other.toInt.toLong))), true)
      case ((left, state, _), Eql(target, other)) => (left, state + (target -> (if (state(target) == state.getOrElse(other, other.toInt.toLong)) 1 else 0)), true)
    }
    require(left.isEmpty || !success)
    Option.when(success)(state("z") == 0)
  }

  var toTest = 99999999999999L
  while (toTest > 99999990000000L) {

    if (run(s"$toTest").contains(true)) {
      println(s"Found: $toTest")
    }
    toTest = toTest - 1
  }

  val options = for {
    w1 <- List(9)
    w2 <- List(9)
    w3 <- List(9)
    w4 <- List(9)
    w5 <- List(9)
    w6 <- (1 to 9)
    w7 <- List(9)
    w8 <- (1 to 9)
    w9 <- (1 to 9)
    w10 <- (1 to 9)
    w11 <- (1 to 9)
    w12 <- (1 to 9)
    w13 <- (1 to 9)
    w14 <- (1 to 9)
    if w7 != w8 + 4
  } yield s"$w1$w2$w3$w4$w5$w6$w7$w8$w9$w10$w11$w12$w13$w14"
  println("Done generating")

  options.par.filter(run(_).contains(true)).foreach(println(_))
  println("Done")

}
