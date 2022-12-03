import scala.io.Source

object Problem03 extends App:
  val input: Vector[String] = Source.fromResource("03-input.txt").getLines().toVector

  val common = input.map(s => s.splitAt(s.length / 2)).map {
    case (s1, s2) => s1.intersect(s2).distinct
  }

  def priority(char: Char): Int = {
    if (char.isUpper) char - 'A' + 27 else char - 'a' + 1
  }

  val solution1 = common.flatMap(_.map(priority)).sum
  println(solution1)

  val badges = input.grouped(3).map(groups => groups(0).intersect(groups(1)).intersect(groups(2)).distinct).toVector

  val solution2 = badges.flatMap(_.map(priority)).sum
  println(solution2)
