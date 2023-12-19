import scala.annotation.tailrec
import scala.collection.immutable.NumericRange
import scala.collection.parallel.CollectionConverters.*
import scala.io.Source

object Problem19 extends App:
  sealed trait RuleResult
  case object Accepted extends RuleResult
  case object Rejected extends RuleResult
  case class Forwarded(workflow: String) extends RuleResult
  object RuleResult:
    def fromString(s: String): RuleResult = s match {
      case "A" => Accepted
      case "R" => Rejected
      case other => Forwarded(other)
    }
  case class Rule(condition: Option[Condition], result: RuleResult)
  case class Workflow(name: String, rules: Vector[Rule])
  case class Part(x: Long, m: Long, a: Long, s: Long):
    def score: Long = x + m + a + s
  def parseField(s: String): Part => Long = s match {
    case "x" => _.x
    case "m" => _.m
    case "a" => _.a
    case "s" => _.s
  }
  case class Condition(field: String, operator: String, threshold: Long):
    val operation: Long => Boolean = operator match {
      case "<" => _ < threshold
      case ">" => _ > threshold
    }
    def applies(part: Part): Boolean = operation(parseField(field)(part))
  object Condition:
    def fromString(s: String): Condition = s match {
    case s"$field>$x" => Condition(field, ">", x.toLong)
    case s"$field<$x" => Condition(field, "<", x.toLong)
  }
  val (workflows, parts) = Source.fromResource("19-input.txt").getLines().toVector.foldLeft((Map.empty[String,Workflow], Vector.empty[Part])) {
    case ((w, p), "") => (w, p)
    case ((w, p), s"{x=$x,m=$m,a=$a,s=$s}") => (w, p.appended(Part(x.toLong, m.toLong, a.toLong, s.toLong)))
    case ((w, p), s"$name{$rules}") => (w + (name -> Workflow(name, rules.split(",").toVector.map {
      case s"$condition:$result" => Rule(Some(Condition.fromString(condition)), RuleResult.fromString(result))
      case result => Rule(None, RuleResult.fromString(result))
    })), p)
  }
//  println(workflows)
//  println(parts)

  @tailrec
  def applyWorkflow(workflow: Workflow, part: Part): RuleResult = {
    val rule = workflow.rules.head
    if rule.condition.forall(_.applies(part)) then
      rule.result match {
        case Accepted | Rejected => rule.result
        case Forwarded(name) => applyWorkflow(workflows(name), part)
      }
    else
      applyWorkflow(workflow.copy(rules = workflow.rules.tail), part)
  }

  val results: Vector[(Part, RuleResult)] = parts.map(part => part -> applyWorkflow(workflows("in"), part))
  println(results.filter(_._2 == Accepted).map(_._1.score).sum)

  def filterEmptyRange(range: NumericRange[Long]): Option[NumericRange[Long]] =
    Option.when(range.nonEmpty)(range)

  def splitRange(range: NumericRange[Long], condition: Condition): (Option[NumericRange[Long]], Option[NumericRange[Long]]) =
    if range.contains(condition.threshold) then
      if condition.operator == ">" then
        (filterEmptyRange(Range.Long.inclusive(condition.threshold+1, range.end, 1)), filterEmptyRange(Range.Long.inclusive(range.start, condition.threshold, 1)))
      else
        (filterEmptyRange(Range.Long.inclusive(range.start, condition.threshold-1, 1)), filterEmptyRange(Range.Long.inclusive(condition.threshold, range.end, 1)))
    else if condition.operation(condition.threshold) then
      (Some(range), None)
    else
      (None, Some(range))


  val allRanges: Map[String, NumericRange[Long]] =
    Vector("x","m","a","s").map(_ -> Range.Long.inclusive(1, 4000, 1)).toMap

  def combinations(ranges: Map[String, NumericRange[Long]]): Long =
    Vector("x","m","a","s").map(ranges).map(_.size.toLong).product

  def acceptedCombinations(workflow: Workflow, ranges: Map[String, NumericRange[Long]]): Long = {
    val rule = workflow.rules.head
    rule.condition match {
      case None => rule.result match {
        case Accepted => combinations(ranges)
        case Rejected => 0L
        case Forwarded(name) => acceptedCombinations(workflows(name), ranges)
      }
      case Some(condition) =>
        val (trueRange, falseRange) = splitRange(ranges(condition.field), condition)
        val falseCombinations = if workflow.rules.tail.isEmpty || falseRange.isEmpty then 0L else acceptedCombinations(workflow.copy(rules = workflow.rules.tail), ranges.updated(condition.field, falseRange.get))
        trueRange.map(range => rule.result match {
          case Accepted => combinations(ranges.updated(condition.field, range)) + falseCombinations
          case Rejected => falseCombinations
          case Forwarded(name) => acceptedCombinations(workflows(name), ranges.updated(condition.field, range)) + falseCombinations
        }).getOrElse(falseCombinations)
    }
  }

  println(acceptedCombinations(workflows("in"), allRanges))
