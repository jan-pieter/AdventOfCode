import scala.io.Source
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers

object Problem18_2 extends App {

  class Calculator1 extends StdTokenParsers with PackratParsers {
    override type Tokens = StdLexical

    override val lexical = new StdLexical

    lexical.delimiters ++= List("(", ")", "+", "-", "*", "/")

    lazy val expr: PackratParser[Long] = addSub

    lazy val addSub: PackratParser[Long] = term * (
      "+" ^^^ { (left: Long, right: Long) => left + right }
        | "-" ^^^ { (left: Long, right: Long) => left - right }
        | "*" ^^^ { (left: Long, right: Long) => left * right }
        | "/" ^^^ { (left: Long, right: Long) => left / right })

    lazy val term: PackratParser[Long] = "(" ~> expr <~ ")" | numericLit ^^ (_.toLong)

    def parse(str: String): Long = expr(new PackratReader(new lexical.Scanner(str))) match {
      case Success(result, remain) if remain.atEnd => result
      case Success(_, remain) => throw new RuntimeException(s"Unparsed input at ${remain.pos}")
      case NoSuccess(msg, remain) => throw new RuntimeException(s"Parse error $msg at ${remain.pos}")
    }
  }

  class Calculator2 extends StdTokenParsers with PackratParsers {
    override type Tokens = StdLexical

    override val lexical = new StdLexical

    lexical.delimiters ++= List("(", ")", "+", "-", "*", "/")

    lazy val expr: PackratParser[Long] = mulDiv

    lazy val addSub: PackratParser[Long] = term * (
      "+" ^^^ { (left: Long, right: Long) => left + right }
        | "-" ^^^ { (left: Long, right: Long) => left - right })

    lazy val mulDiv: PackratParser[Long] = addSub * (
      "*" ^^^ { (left: Long, right: Long) => left * right }
        | "/" ^^^ { (left: Long, right: Long) => left / right })

    lazy val term: PackratParser[Long] = "(" ~> expr <~ ")" | numericLit ^^ (_.toLong)

    def parse(str: String): Long = expr(new PackratReader(new lexical.Scanner(str))) match {
      case Success(result, remain) if remain.atEnd => result
      case Success(_, remain) => throw new RuntimeException(s"Unparsed input at ${remain.pos}")
      case NoSuccess(msg, remain) => throw new RuntimeException(s"Parse error $msg at ${remain.pos}")
    }
  }

  val input = Source.fromResource("18-input.txt").getLines().toList

  val calculator1 = new Calculator1
  println(input.map(calculator1.parse).sum)
  println()

  val calculator2 = new Calculator2
  println(input.map(calculator2.parse).sum)

}
