import scala.io.Source

object Problem02 extends App {

  val boxes = Source.fromResource("02-input.txt").getLines().toList

  val histoValues = boxes.map(_.groupBy(identity).view.mapValues(_.length))

  val twoTimes = histoValues.count(_.values.toSet.contains(2))
  val threeTimes = histoValues.count(_.values.toSet.contains(3))

  println(s"Two times: $twoTimes, three times: $threeTimes, checksum: ${twoTimes * threeTimes}")

  def checkMatch(str1: String, str2: String): Unit = {
    val diff = str1.zip(str2).count(t => t._1 != t._2)
//    println(s"Match: $str1 $str2 Diff: $diff")
    if (diff == 1) {
      val same = str1.zip(str2).filter(t => t._1 == t._2).map(_._1).mkString
      println(s"Match: $str1 $str2 Same: $same")
    }
  }

  boxes.combinations(2).foreach(combo => checkMatch(combo(0), combo(1)))
}
