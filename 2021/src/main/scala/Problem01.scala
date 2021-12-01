import scala.io.Source

object Problem01 extends App {

  val input = Source.fromResource("01-input.txt").getLines().map(_.toInt).toVector

  val tuples = input.sliding(2).map(v => v(0) -> v(1))

  println(tuples.count(t => t._1 < t._2))

  val tuples3 = input.sliding(3).map(v => v(0) + v(1) + v(2)).sliding(2).map(v => v(0) -> v(1))

  println(tuples3.count(t => t._1 < t._2))
}
