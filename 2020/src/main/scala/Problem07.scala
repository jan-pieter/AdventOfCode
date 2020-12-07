import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex

object Problem07 extends App {

  case class Rule(color: String, contents: List[(Int, String)])
  object Rule {
    private val emptyRegex: Regex = "^(.*) bags contain no other bags.$".r
    private val nonEmptyRegex: Regex = "^(.*) bags contain (.*).$".r
    private val contentsRegex = "^([\\d]+) (.*) bag(s?)".r
    def apply(str: String): Rule = {
      str match {
        case emptyRegex(color) => Rule(color, List.empty)
        case nonEmptyRegex(color, contents) => Rule(color, contents.split(", ").map {
          case contentsRegex(amount, color, _) => amount.toInt -> color
        }.toList)
      }
    }
  }

  val input = Source.fromResource("07-input.txt").getLines().map(Rule(_)).toList
  println(input)

  val todo = mutable.Queue("shiny gold")
  val done = mutable.Set[String]()
  while(todo.nonEmpty) {
    val color = todo.dequeue()
    input.filter(rule => rule.contents.exists(_._2 == color) && !done.contains(rule.color)).foreach { rule =>
      todo.enqueue(rule.color)
      done.add(rule.color)
    }
  }

  println(done.size)

  def innerBags(color: String): Int = {
    val rule = input.find(_.color == color).getOrElse(throw new IllegalStateException(s"Rule for color $color not found"))
    rule.contents.map {
      case (amount, innerColor) => amount * (1+innerBags(innerColor))
    }.sum
  }

  println(innerBags("shiny gold"))

}
