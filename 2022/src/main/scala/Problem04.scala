import scala.io.Source

object Problem04 extends App:
  case class AssignmentPair(aMin: Int, aMax: Int, bMin: Int, bMax: Int) {
    val fullyContainsOne: Boolean = (aMin >= bMin && aMax <= bMax) || (bMin >= aMin && bMax <= aMax)
    val overlap: Boolean = (bMin <= aMax && aMin <= bMin) || (aMin <= bMax && bMin <= aMin)
  }
  val input = Source.fromResource("04-test.txt").getLines().toVector.map {
    case s"$a-$b,$c-$d" => AssignmentPair(a.toInt, b.toInt, c.toInt, d.toInt)
  }

  val solution1 = input.count(_.fullyContainsOne)
  println(solution1)

  val solution2 = input.count(_.overlap)
  println(solution2)
