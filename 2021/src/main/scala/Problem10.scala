import scala.annotation.tailrec
import scala.io.Source

object Problem10 extends App {

  val input: Vector[String] = Source.fromResource("10-input.txt").getLines().toVector

  @tailrec
  def resolve(str: String): String = {
    val result = str.replace("<>","").replace("[]", "").replace("{}", "").replace("()", "")
    if (result == str)
      result
    else
      resolve(result)
  }

  val resolved = input.map(resolve)
  resolved.foreach(println(_))

  val closingChars = Set(']', '}', '>', ')')

  val score1 = resolved.flatMap(_.find(closingChars.contains)).map {
    case ')' => 3
    case ']' => 57
    case '}' => 1197
    case '>' => 25137
  }.sum

  println(s"Solution1 $score1")

  val incomplete = resolved.filter(!_.exists(closingChars.contains))

  incomplete.foreach(println(_))
  println("")

  val solutions = incomplete.map(_.replace('<', '>').replace('(',')').replace('[', ']').replace('{', '}').reverse)
  solutions.foreach(println(_))

  def score(str: String): Long = {
    str.foldLeft(0L){
      case (score, ')') => score * 5 + 1
      case (score, ']') => score * 5 + 2
      case (score, '}') => score * 5 + 3
      case (score, '>') => score * 5 + 4
    }
  }

  val scores = solutions.map(score)

  scores.foreach(println(_))

  val winning = scores.sorted.apply(scores.length / 2)
  println(s"Solution 2: $winning")
}
