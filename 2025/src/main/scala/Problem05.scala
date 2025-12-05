import scala.io.Source

object Problem05 extends App:
//  val file = "05-test.txt"
  val file = "05-input.txt"
  val input = Source.fromResource(file).getLines().toVector

  val (freshRanges, available) = input.foldLeft(Vector.empty[(Long, Long)] -> Vector.empty[Long]) {
    case (f, a) -> "" => (f, a)
    case (f, a) -> s"$start-$end" => f.appended(start.toLong -> end.toLong) -> a
    case (f, a) -> i => (f, a.appended(i.toLong))
  }
//  println(freshRanges)
//  println(available)

  def isFresh(item: Long): Boolean = freshRanges.exists((start, end) => item >= start && item <= end)
  println(available.count(isFresh))

  val joinedRanges = freshRanges.sortBy(_._1).foldLeft(Vector.empty[(Long, Long)]) {
    case (acc, range) if acc.isEmpty => acc.prepended(range)
    case (acc, start -> end) if start <= acc.head._2 && end <= acc.head._2 => acc
    case (acc, start -> end) if start <= acc.head._2 => acc.drop(1).prepended(acc.head._1 -> end)
    case (acc, start -> end) => acc.prepended(start -> end)
  }
//  println(joinedRanges)
  println(joinedRanges.map((start, end) => end-start+1).sum)
