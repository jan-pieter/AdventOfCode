import scala.io.Source

object Problem01 extends App:
  val input = Source.fromResource("01-input.txt").getLines().toVector
  val result = input.map(_.filter(_.isDigit)).map(x => (x.take(1) + x.takeRight(1)).toInt)
  println(result.sum)

  val replacements = Map("one" -> "1", "two" -> "2", "three" -> "3", "four" -> "4", "five" -> "5", "six" -> "6", "seven" -> "7", "eight" -> "8", "nine" -> "9")
  val amended = input.map(str => replacements.foldLeft(str)((s, tuple) => s.replace(tuple._1, tuple._1.take(1) + tuple._2 + tuple._1.takeRight(1))))
  val result2 = amended.map(_.filter(_.isDigit)).map(x => (x.take(1) + x.takeRight(1)).toInt)
  //println(result2.mkString(","))
  println(result2.sum)
