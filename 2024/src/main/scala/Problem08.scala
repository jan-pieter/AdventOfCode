import scala.io.Source

object Problem08 extends App:
//  val file = "08-test.txt"
  val file = "08-input.txt"
  val input = Source.fromResource(file).getLines().toVector.map(_.toVector)

  val antennas: Map[Char, Vector[(Int, Int)]] = (for {
    y <- input.indices
    x <- input(y).indices
  } yield (y, x) ).foldLeft(Map.empty[Char, Vector[(Int, Int)]]) { case (acc, (y, x)) =>
    val c = input(y)(x)
    if c == '.' then acc else acc.updated(c, acc.getOrElse(c, Vector.empty) :+ (y, x))
  }
  //antennas.foreach(println)

  val antinodes = antennas.map((c, coords) => coords.combinations(2).toVector.flatMap{
    case Vector(a, b) =>
      val dy = a._1 - b._1
      val dx = a._2 - b._2
      val antinode1 = (a._1 + dy, a._2 + dx)
      val antinode2 = (b._1 - dy, b._2 - dx)
      Vector(antinode1, antinode2)
  })
  val result = antinodes.reduce(_ ++ _).toSet.filter((y, x) => x >= 0 && y >= 0 && x < input.head.length && y < input.length)
  println(result.size)

  val maxSize = Math.max(input.length, input.head.length)
  val antinodes2 = antennas.map((c, coords) => coords.combinations(2).toVector.flatMap {
    case Vector(a, b) =>
      val dy = a._1 - b._1
      val dx = a._2 - b._2
      (0 to maxSize).flatMap { i =>
        val antinode1 = (a._1 + i * dy, a._2 + i * dx)
        val antinode2 = (b._1 - i * dy, b._2 - i * dx)
        Vector(antinode1, antinode2)
      }
  })

  val result2 = antinodes2.reduce(_ ++ _).toSet.filter((y, x) => x >= 0 && y >= 0 && x < input.head.length && y < input.length)
  println(result2.size)
