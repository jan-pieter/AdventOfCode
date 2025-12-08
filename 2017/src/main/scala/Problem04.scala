import scala.io.Source

object Problem04 extends App {

  val input = Source.fromResource("04-input.txt").getLines().toList.map(_.split(" "))

  val withoutDuplicates = input.filter(words => words.length == words.distinct.length)

  println(withoutDuplicates.size)

  val withoutAnagrams = input.filter(words => words.length == words.map(word => word.toList.groupBy(c => c).view.mapValues(_.size)).toSet.size)

  println(withoutAnagrams.size)

}
