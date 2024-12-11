import scala.io.Source

object Problem11 extends App:
//  val file = "11-test.txt"
  val file = "11-input.txt"
  val input = Source.fromResource(file).getLines().toVector.head.split(" ").map(_.toLong).toVector

  def blink(stone: Long): Vector[Long] =
    if stone == 0 then Vector(1)
    else if stone.toString.length % 2 == 0 then stone.toString.splitAt(stone.toString.length / 2) match
      case (a, b) => Vector(a.toLong, b.toLong)
    else Vector(stone*2024)

  val result = (1 to 25).foldLeft(input)((acc, _) => acc.flatMap(blink))
  println(result.length)

  val input2: Map[Long, Long] = input.groupBy(identity).view.mapValues(_.size.toLong).toMap

  def blink2(stones: Map[Long, Long]): Map[Long, Long] =
    stones.toVector.flatMap {
      case (stone, times) => blink(stone).groupBy(identity).view.mapValues(_.size * times).toVector
    }.groupBy(_._1).view.mapValues(_.map(_._2).sum).toMap

  val result2 = (1 to 75).foldLeft(input2)((acc, _) => blink2(acc))

  println(result2.values.sum)
