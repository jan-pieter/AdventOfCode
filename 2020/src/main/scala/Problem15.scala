import scala.collection.mutable
import scala.io.Source

object Problem15 extends App {

  val input = Source.fromResource("15-input.txt").getLines().next().split(",").map(_.toInt)

  val seen: mutable.Map[Int, Int] = mutable.Map.empty
  var lastNumber = 0

  (0 until 30000000).foreach { turn =>
    if (turn < input.length) {
      if (turn != 0)
        seen.put(lastNumber, turn - 1)
      lastNumber = input(turn)
    } else {
      if (!seen.contains(lastNumber)) {
        seen.put(lastNumber, turn - 1)
        lastNumber = 0
      } else {
        val when = seen(lastNumber)
        seen.put(lastNumber, turn - 1)
        lastNumber = turn - 1 - when
      }
    }
//    println(s"$turn $lastNumber")
  }
  println(lastNumber)

}
