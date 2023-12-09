import scala.io.Source

object Problem09 extends App:
  val input = Source.fromResource("09-input.txt").getLines().toVector.map(_.split(" ").toVector.map(_.toLong))

  def extrapolate(line: Vector[Long]): Long =
    val diffs = line.sliding(2).toVector.map:
      case Vector(left, right) => right - left
    if diffs.forall(_==0L) then line.last else line.last + extrapolate(diffs)

  println(input.map(extrapolate).sum)

  println(input.map(_.reverse).map(extrapolate).sum)