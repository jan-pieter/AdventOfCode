import scala.io.Source

object Problem01 extends App {
  val input = Source.fromResource("01-input.txt").getLines().map(_.toInt).toVector
  val solution1 = input.sliding(2).count(window => window(0) < window(1))
  println(solution1)

  val solution2 = input.sliding(3).map(_.sum).sliding(2).count(window => window(0) < window(1))
  println(solution2)
}
