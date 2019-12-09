import scala.io.Source

object Problem01 extends App {

  println(Source.fromResource("01-input.txt").getLines().map(_.toLong).sum)

  case class Intermediate(frequency: Long, seen: Set[Long])

  val changes: Vector[Long] = Source.fromResource("01-input.txt").getLines().map(_.toLong).toVector

  Iterator.from(0).map(i => changes(i % changes.size)).foldLeft(Intermediate(0L, Set.empty))((soFar, change) => {
    val newFrequency = soFar.frequency + change
    if (soFar.seen.contains(newFrequency)) {
      println(s"Seen twice: $newFrequency")
      sys.exit()
    }
    Intermediate(newFrequency, soFar.seen + newFrequency)
  })
}
