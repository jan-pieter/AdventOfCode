import scala.io.Source

object Problem11 extends App:
  val input = Source.fromResource("11-input.txt").getLines().toVector

  def duplicateEmptyRows(v: Vector[Vector[Char]]): Vector[Vector[Char]] = v.flatMap {
    case line if line.forall(_ == '.') => Vector(line, line)
    case line => Vector(line)
  }

  val expanded = duplicateEmptyRows(duplicateEmptyRows(input.map(_.toVector)).transpose).transpose
  val galaxies = expanded.zipWithIndex.flatMap((line, row) => line.zipWithIndex.filter((c, i) => c == '#').map((c, i) => (row, i)))
  val distances = galaxies.combinations(2).map {
    case Vector((r1, c1), (r2, c2)) => (r1 - r2).abs + (c1 - c2).abs
  }
  println(distances.sum)

  def emptyLines(v: Vector[Vector[Char]]): Set[Int] =
    v.zipWithIndex.filter((line, i) => line.forall(_ == '.')).map(_._2).toSet

  val emptyRows = emptyLines(input.map(_.toVector))
  val emptyColumns = emptyLines(input.transpose)
  val multiplier = BigInt(1000000)

  def distance(x1: Int, x2: Int, empty: Set[Int]): BigInt = {
    val r = if (x1 <= x2) x1 until x2 else x2 until x1
    r.map {
      case x if empty(x) => multiplier
      case x => BigInt(1)
    }.sum
  }

  val galaxies2 = input.zipWithIndex.flatMap((line, row) => line.zipWithIndex.filter((c, i) => c == '#').map((c, i) => (row, i)))
  val distances2 = galaxies2.combinations(2).map {
    case Vector((r1, c1), (r2, c2)) => distance(r1, r2, emptyRows) + distance(c1, c2, emptyColumns)
  }
  println(distances2.sum)
