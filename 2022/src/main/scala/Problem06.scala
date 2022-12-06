import scala.io.Source

object Problem06 extends App:
  val input = Source.fromResource("06-input.txt").getLines().toVector.head

  def firstAllDifferent(string: String, length: Int): Int =
    string.sliding(length).zipWithIndex.find(_._1.distinct.length == length).map(_._2 + length).getOrElse(-1)

  println(firstAllDifferent(input, 4))

  println(firstAllDifferent(input, 14))
