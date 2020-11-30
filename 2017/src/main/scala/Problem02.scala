import java.math.MathContext

import scala.io.Source

object Problem02 extends App {

  val input = Source.fromResource("02-input.txt").getLines().toList.map(_.replace("\t", " ").split(" ").map(_.toInt))

  val checksum = input.map(values => values.max - values.min).sum

  println(checksum)

  def result(arr: Array[Int]): Int = {
    val sorted: Array[Int] = arr.sorted.reverse
    val results = for {
      i <- sorted.indices
      j <- i+1 until sorted.length
    } yield {
      val x = BigDecimal(sorted(i)) / BigDecimal(sorted(j))
      //println(s"$i $j $x")
      x
    }
    results.find(_.isValidInt).map(_.toInt).getOrElse(0)
  }

  val res = input.map(result).sum

  println(res)
}
