import scala.collection.mutable
import scala.io.Source
import scala.util.Try

object Problem20_2  extends App {

  val input = Source.fromResource("20-input.txt").getLines().toList

  def split(list: List[String]): List[List[String]] = {
    val result = list.foldLeft(List(List.empty[String])) {
      case (state, "") => List.empty[String] :: state
      case (state, input) => (input :: state.head) :: state.tail
    }
    result.reverse.map(_.reverse)
  }
  case class Tile(id: Int, contents: Array[Array[Char]]) {
    val borders: List[String] = {
      List(
        contents(0).mkString(""),
        contents(contents.length - 1).mkString("").reverse,
        contents.map(_.apply(0)).mkString("").reverse,
        contents.map(_.apply(contents.length - 1)).mkString("")
      )
    }

    val possibleBorders: List[String] = borders.flatMap(border =>
      List(border, border.reverse)
    )

    val leftBorder: String = contents.map(_.apply(0)).mkString("")
    val rightBorder: String = contents.map(_.apply(contents.length - 1)).mkString("")
    val topBorder: String = contents(0).mkString("")
    val bottomBorder: String = contents(contents.length - 1).mkString("")

    def transformed(rotations: Int, flipped: Boolean): Tile = {
      copy(contents = transform(contents, rotations, flipped))
    }
  }

  val splittedInput = split(input)

  val tiles: List[Tile] = splittedInput.map(_.filterNot(_.isEmpty)).map { lines =>
    Tile(
      lines.head.filter(_.isDigit).toInt,
      lines.tail.map(_.toCharArray).toArray
    )
  }

  println(s"${tiles.length} tiles")

  val corners = tiles.flatMap { tile =>
    val otherBorders = tiles.filter(_.id != tile.id).flatMap(_.possibleBorders).toSet
    val unmatchedBorders = tile.borders.filter(border => !otherBorders.contains(border))
    if (unmatchedBorders.length == 2)
      Some(tile)
    else
      None
  }

  println(corners.length)
  println(corners.map(_.id.toLong).product)

  def reverseRows(m: Array[Array[Char]]): Array[Array[Char]] = m.map(_.reverse)

  def rotate(in: Array[Array[Char]]): Array[Array[Char]] = {
    def transpose(m: Array[Array[Char]]): Array[Array[Char]] = {
      val out = Array.ofDim[Char](m.length, m.length)
      for {
        c <- m.indices
        r <- m.indices
      } yield {
        out(c)(r) = m(r)(c)
      }
      out
    }
    reverseRows(transpose(in))
  }

  def transform(contents: Array[Array[Char]], rotations: Int, flipped: Boolean): Array[Array[Char]] = {
    val rotated = (0 until rotations).foldLeft(contents) { case (matrix, _) => rotate(matrix)}
    if (flipped)
      reverseRows(rotated)
    else
      rotated
  }

  val imageLength = Math.sqrt(tiles.length).toInt
  println(s"imageLength = $imageLength")

  def rotatedAndFlipped(tile: Tile, leftBorder: Option[String], topBorder: Option[String]): Tile = {
    val possibilities = for {
      rotations <- 0 until 4
      flipped <- List(false, true)
    } yield tile.transformed(rotations, flipped)
    possibilities.filter { tile =>
      leftBorder.forall(_ == tile.leftBorder) && topBorder.forall(_ == tile.topBorder)
    }.head
  }

  def trySolution(rotated: Int, flipped: Boolean): Option[Array[Array[Tile]]] = Try {
    val imageWithTiles: Array[Array[Tile]] = Array.ofDim[Tile](imageLength, imageLength)
    val tilesLeft: mutable.Set[Tile] = mutable.Set(tiles*)

    for {
      y <- imageWithTiles.indices
      x <- imageWithTiles.indices
    } yield {
      if (x == 0 && y == 0) {
        imageWithTiles(y)(x) = corners.head.transformed(rotated, flipped)
        tilesLeft.remove(corners.head)
      } else if (y == 0) {
        // Only match left
        val toMatch = imageWithTiles(0)(x - 1).rightBorder
        val selectedTile: Tile = tilesLeft.find(tile => tile.possibleBorders.contains(toMatch)).head
        imageWithTiles(y)(x) = rotatedAndFlipped(selectedTile, Some(toMatch), None)
        tilesLeft.remove(selectedTile)
      } else if (x == 0) {
        // Only match top
        val toMatch = imageWithTiles(y - 1)(0).bottomBorder
        val selectedTile: Tile = tilesLeft.find(tile => tile.possibleBorders.contains(toMatch)).head
        imageWithTiles(y)(x) = rotatedAndFlipped(selectedTile, None, Some(toMatch))
        tilesLeft.remove(selectedTile)
      } else {
        // Match left & top
        val toMatchL = imageWithTiles(y)(x - 1).rightBorder
        val toMatchT = imageWithTiles(y - 1)(x).bottomBorder
        val selectedTile: Tile = tilesLeft.find(tile => tile.possibleBorders.contains(toMatchL) && tile.possibleBorders.contains(toMatchT)).head
        imageWithTiles(y)(x) = rotatedAndFlipped(selectedTile, Some(toMatchL), Some(toMatchT))
        tilesLeft.remove(selectedTile)
      }
    }
    imageWithTiles
  }.toOption

  def flattenSolution(solution: Array[Array[Tile]]): Array[Array[Char]] = {
    solution.flatMap { row =>
      (1 until row.head.contents.length - 1).map { rowIndex =>
        row.map(_.contents(rowIndex).drop(1).dropRight(1)).reduce(_ ++ _)
      }
    }
  }

  (for {
    rotated <- 0 until 4
    flipped <- List(true, false)
  } yield {
    trySolution(rotated, flipped)
  }).flatten.map { solution =>
    println("New solution:")
    /*solution.foreach { row =>
      row.head.contents.indices.foreach { rowIndex =>
        row.foreach { tile =>
          print(tile.contents(rowIndex).mkString(""))
          print(" ")
        }
        print("\n")
      }
      println()
    }*/
    val flattened = flattenSolution(solution)

    (for {
      rotations <- 0 until 4
      flipped <- List(false, true)
    } yield transform(flattened, rotations, flipped)).foreach { flattened =>
      val monsters = (for {
        x <- 0 until flattened.head.length - 19
        y <- 0 until flattened.length - 2
      } yield {
        if (
          flattened(y)(x + 18) == '#' &&
            flattened(y + 1)(x) == '#' &&
            flattened(y + 1)(x + 5) == '#' &&
            flattened(y + 1)(x + 6) == '#' &&
            flattened(y + 1)(x + 11) == '#' &&
            flattened(y + 1)(x + 12) == '#' &&
            flattened(y + 1)(x + 17) == '#' &&
            flattened(y + 1)(x + 18) == '#' &&
            flattened(y + 1)(x + 19) == '#' &&
            flattened(y + 2)(x + 1) == '#' &&
            flattened(y + 2)(x + 4) == '#' &&
            flattened(y + 2)(x + 7) == '#' &&
            flattened(y + 2)(x + 10) == '#' &&
            flattened(y + 2)(x + 13) == '#' &&
            flattened(y + 2)(x + 16) == '#'
        ) 1 else 0
      }).sum
      println(s"$monsters monsters found")

      val totalHashes = flattened.map(_.count(_ == '#')).sum
      println(s"Total hashes: $totalHashes")
      println(s"Minus seamonsters: ${totalHashes - monsters * 15}")
    }
    //println("Flattened:")
    //flattened.foreach(line => println(line.mkString("")))
  }
}
