import scala.collection.mutable
import scala.io.Source

object Problem10 extends App {

  val input = Source.fromResource("10-input.txt").getLines().toList.map(_.toInt)

  val diffs = (0 :: input).sorted.sliding(2).map {
    case List(a, b) => b - a
  }.toList

  val diff3 = diffs.count(_ == 3) + 1
  val diff1 = diffs.count(_ == 1)

  println(s"${diff3} ${diff1} ${diff3 * diff1}")

  val maxJolts = input.max
  val cache : mutable.Map[Int, Long] = mutable.Map.empty
  def countArrangements(jolts: Int): Long = jolts match {
    case jolts if jolts == maxJolts => 1L
    case jolts if cache.contains(jolts) => cache(jolts)
    case jolts =>
      val validAdapters = input.filter(target => target - jolts > 0 && target - jolts <= 3)
      val result = validAdapters.map(countArrangements).sum
      cache.put(jolts, result)
      result
  }

  println(countArrangements(0))
}
