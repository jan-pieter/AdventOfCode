import scala.io.Source

object Problem25 extends App:
  val input = Source.fromResource("25-input.txt").getLines().toVector

  def toDecimal(c: Char): Long = c match {
    case '=' => -2L
    case '-' => -1L
    case other => other.asDigit.toLong
  }

  def toDecimal(s: String): Long = {
    s.reverse.zipWithIndex.map((c, i) => toDecimal(c) * scala.math.pow(5, i).toLong).sum
  }

  val snafuChars: Vector[String] = Vector("0", "1", "2", "=", "-")
  def toSnafu(i: Long): String = {
    val x = i % 5
    val toAdd = x match {
      case 3 | 4 => 1
      case _ => 0
    }
    val newSnafu = i / 5 + toAdd
    if (newSnafu > 0)
      toSnafu(newSnafu) + snafuChars(x.toInt)
    else
      snafuChars(x.toInt)
  }

  val solution1 = input.map(toDecimal).sum
  println(toSnafu(solution1))
