import scala.io.Source

object Problem19 extends App {

  val input = Source.fromResource("19-input.txt").getLines().toList

  def split(list: List[String]): List[List[String]] = {
    val result = list.foldLeft(List(List.empty[String])) {
      case (state, "") => List.empty[String] :: state
      case (state, input) => (input :: state.head) :: state.tail
    }
    result.reverse.map(_.reverse)
  }


  trait Rule {
    def number: Int
    def matches(input: String): (Boolean, String)
  }

  case class DoubleRefRule(number: Int, seq1: Seq[Int], seq2: Seq[Int]) extends Rule {
    override def matches(input: String): (Boolean, String) =
      seq1.map(rules2).foldLeft((true, input)) {
        case ((true, in), nextRule) => nextRule.matches(in)
        case ((false, _), _) => false -> input
      } match {
        case (true, str) => true -> str
        case _ => seq2.map(rules2).foldLeft((true, input)) {
          case ((true, in), nextRule) => nextRule.matches(in)
          case ((false, _), _) => false -> input
        }
      }
  }
  case class SingleRefRule(number: Int, seq: Seq[Int]) extends Rule {
    override def matches(input: String): (Boolean, String) = seq.map(rules2).foldLeft((true, input)) {
      case ((true, in), nextRule) => nextRule.matches(in)
      case ((false, _), _) => false -> input
    }
  }
  case class StaticRule(number: Int, str: String) extends Rule {
    override def matches(input: String): (Boolean, String) = input.take(str.length) match {
      case part if part == str => true -> input.drop(str.length)
      case _ => false -> input
    }
  }
  case class Rule8() extends Rule {
    override val number: Int = 8
    override def matches(input: String): (Boolean, String) = {
      var result = rules2(42).matches(input)
      if (result._1) {
        var done = false
        while (!done) {
          rules2(42).matches(result._2) match {
            case (true, str) => result = (true, str)
            case (false, str) =>
              result = (true, result._2)
              done = true
          }
        }
      }
      result
    }
    def matchOptions(input: String): List[(Boolean, String)] = {
      var allResults = List.empty[(Boolean, String)]
      var result = rules2(42).matches(input)
      if (result._1) {
        var done = false
        while (!done) {
          allResults = result :: allResults
          rules2(42).matches(result._2) match {
            case (true, str) => result = (true, str)
            case (false, str) =>
              done = true
          }
        }
      }
      allResults
    }

  }
  case class Rule11() extends Rule {
    override val number: Int = 11
    override def matches(input: String): (Boolean, String) = {
      var result = rules2(42).matches(input)
      var matches = 1
      if (result._1) {
        var allResults = List.empty[(Int, String)]
        var done = false
        while (!done) {
          allResults = (matches, result._2) :: allResults
          rules2(42).matches(result._2) match {
            case (true, str) =>
              result = (true, str)
              matches = matches + 1
            case (false, str) =>
              result = (true, result._2)
              done = true
          }
        }

        if (allResults.exists {
          case (times, str) =>
            result = (false, str)
            var todo = times
            while (todo > 0) {
              result = rules2(31).matches(result._2)
              todo = todo - 1
            }
            result == (true, "")
        }) true -> "" else false -> input


      } else false -> input
    }
  }

  case class Rule0() extends Rule {
    override def number: Int = 0

    override def matches(input: String): (Boolean, String) = {
      val options = Rule8().matchOptions(input)
      if (options.nonEmpty) {
        if (options.exists {
          case (_, left) => Rule11().matches(left) == (true, "")
        }) true -> "" else false -> input
      } else false -> input

    }
  }

  val splittedInput = split(input)

  val staticRegex = "^(\\d+): \"([ab])\"$".r
  val singleRefRegex = "^(\\d+): ([0-9 ]+)$".r
  val doubleRefRegex = "^(\\d+): ([0-9 ]+) \\| ([0-9 ]+)$".r
  val rules: Map[Int, Rule] = splittedInput(0).map {
    case staticRegex(number, value) => StaticRule(number.toInt, value)
    case singleRefRegex(number, rules) => SingleRefRule(number.toInt, rules.split(" ").toList.map(_.toInt))
    case doubleRefRegex(number, part1, part2) => DoubleRefRule(number.toInt, part1.split(" ").toList.map(_.toInt), part2.split(" ").toList.map(_.toInt))
  }.map(rule => rule.number -> rule).toMap

  val instances = splittedInput(1).filterNot(_.isEmpty)

  //println(instances.count(str => rules(0).matches(str) == (true -> "")))

  val rules2 = rules ++ Map(8 -> Rule8(), 11 -> Rule11(), 0 -> Rule0())//, 42 -> StaticRule(42, "x"), 31 -> StaticRule(31, "y"))

  //val testInstances = List("xxy", "xxxyy", "xx")
  //println(testInstances.map(rules2(0).matches))

//  println(rules2(8))
//  println(rules2(11))
//  println(rules2(42))
//
  println(instances.count(str => rules2(0).matches(str) == (true -> "")))

}
