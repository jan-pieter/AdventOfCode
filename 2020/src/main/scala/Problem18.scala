import scala.io.Source

object Problem18 extends App {
  val input = Source.fromResource("18-input.txt").getLines().toList

  val parens = "\\(([0-9 \\+\\*]*)\\)".r
  case class State(num: Long, op: Option[String])
  def eval(str: String): Long = {
    parens.findFirstIn(str) match {
      case Some(parenstr) =>
        eval(parens.replaceFirstIn(str, eval(parenstr.drop(1).dropRight(1)).toString))

      case None => str.split(" ").foldLeft(State(0, Some("+"))) {
        case (State(num, Some("+")), newNum) => State(num + newNum.toLong, None)
        case (State(num, Some("*")), newNum) => State(num * newNum.toLong, None)
        case (State(num, None), newOp) => State(num, Some(newOp))
      }.num
    }
  }

  println(input.map(eval).sum)

  val addition = "(\\d+) \\+ (\\d+)".r
  val onlyAddition = "^(\\d+) \\+ (\\d+)$".r
  def eval2(str: String): Long = {
    parens.findFirstIn(str) match {
      case Some(parenstr) =>
        eval2(parens.replaceFirstIn(str, eval2(parenstr.drop(1).dropRight(1)).toString))

      case None =>
        if (onlyAddition.findFirstIn(str).isDefined || addition.findFirstIn(str).isEmpty) {
          str.split(" ").foldLeft(State(0, Some("+"))) {
            case (State(num, Some("+")), newNum) => State(num + newNum.toLong, None)
            case (State(num, Some("*")), newNum) => State(num * newNum.toLong, None)
            case (State(num, None), newOp) => State(num, Some(newOp))
          }.num
        } else {
          val replacement = addition.replaceAllIn(str, "($0)")
          eval2(replacement)
        }
    }
  }

  println(input.map(eval2).sum)

}
