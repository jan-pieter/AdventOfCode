import scala.collection.{Set, mutable}
import scala.io.Source
import scala.util.Try

object Problem20 extends App {

  val input = Source.fromResource("20-test.txt").getLines().toList

  def split(list: List[String]): List[List[String]] = {
    val result = list.foldLeft(List(List.empty[String])) {
      case (state, "") => List.empty[String] :: state
      case (state, input) => (input :: state.head) :: state.tail
    }
    result.reverse.map(_.reverse)
  }

  case class Border(str: String, rotations: Int, flipped: Boolean)
  case class Tile(id: Int, contents: Array[Array[Char]]) {
    val borders: List[Border] = {
      List(
        Border(contents(0).mkString(""), 0, flipped = false),
        Border(contents(contents.length - 1).mkString("").reverse, 2, flipped = false),
        Border(contents.map(_.apply(0)).mkString("").reverse, 1, flipped = false),
        Border(contents.map(_.apply(contents.length - 1)).mkString(""), 3, flipped = false)
      )
    }

    val possibleBorders: List[Border] = borders.flatMap(border =>
      List(
        border, border.copy(str = border.str.reverse, flipped = true)
      )
    )

    def border(rotations: Int, flipped: Boolean): Border =
      possibleBorders.find(border => border.rotations == rotations && border.flipped == flipped).get
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
    val otherBorders = tiles.filter(_.id != tile.id).flatMap(_.possibleBorders.map(_.str)).toSet
    val unmatchedBorders = tile.borders.filter(border => !otherBorders.contains(border.str))
    val unmatchedRotations = unmatchedBorders.map(_.rotations).toSet
    if (unmatchedBorders.length == 2) {
      val tileRotations = unmatchedRotations match {
        case set if set(0) && set(1) => 0
        case set if set(1) && set(2) => 1
        case set if set(2) && set(3) => 2
        case set if set(3) && set(0) => 3
      }
      Some(RotatedAndFlipped(tile, tileRotations, flipped = false))
    } else
      None
  }

  //println(corners)
  println(corners.length)
  //println(corners.map(_.tile.id.toLong).product)
  corners.foreach(corner => println(s"${corner.rightBorder} ${corner.bottomBorder}"))

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

  case class RotatedAndFlipped(tile: Tile, rotations: Int, flipped: Boolean) {
    val contents: Array[Array[Char]] = {
      val rotated = (0 until rotations).foldLeft(tile.contents) { case (matrix, _) => rotate(matrix)}
      if (flipped)
        reverseRows(rotated)
      else
        rotated
    }
    val rightBorder: String = contents.map(_.apply(contents.length - 1)).mkString("")
    val bottomBorder: String = contents(contents.length - 1).mkString("")
  }

  val imageLength = Math.sqrt(tiles.length).toInt
  println(s"imageLength = $imageLength")

  def trySolution(rotated: Int, flipped: Boolean): Option[Array[Array[RotatedAndFlipped]]] = Try {
    val imageWithTiles: Array[Array[RotatedAndFlipped]] = Array.ofDim[RotatedAndFlipped](imageLength, imageLength)
    val tilesLeft: mutable.Set[Tile] = mutable.Set(tiles: _*)

    for {
      y <- imageWithTiles.indices
      x <- imageWithTiles.indices
    } yield {
      if (x == 0 && y == 0) {
        imageWithTiles(y)(x) = RotatedAndFlipped(corners.head.tile, rotated, flipped)
//        println(corners.head)
        tilesLeft.remove(corners.head.tile)
      } else if (y == 0) {
        // Only match left
        val toMatch = imageWithTiles(0)(x - 1).rightBorder
        val selectedTile: (Tile, Border) = tilesLeft.flatMap(tile => tile.possibleBorders.find(b => b.str == toMatch) match {
          case None => None
          case Some(border) => Some(tile -> border)
        }).head
        imageWithTiles(y)(x) = RotatedAndFlipped(selectedTile._1, (selectedTile._2.rotations + 3) % 4, selectedTile._2.flipped)
        tilesLeft.remove(selectedTile._1)
      } else if (x == 0) {
        // Only match top
        val toMatch = imageWithTiles(y - 1)(0).bottomBorder
        val selectedTile: (Tile, Border) = tilesLeft.flatMap(tile => tile.possibleBorders.find(b => b.str == toMatch) match {
          case None => None
          case Some(border) => Some(tile -> border)
        }).head
        imageWithTiles(y)(x) = RotatedAndFlipped(selectedTile._1, selectedTile._2.rotations, selectedTile._2.flipped)
        tilesLeft.remove(selectedTile._1)
      } else {
        // Match left & top
//        val toMatchL = imageWithTiles(y)(x - 1).rightBorder
//        val toMatchT = imageWithTiles(y - 1)(x).bottomBorder
//        val selectedTile: (Tile, Border) = tilesLeft.flatMap(tile => tile.possibleBorders.find(b => b.str == toMatchT) match {
//          case None => None
//          case Some(border) =>
//            val rotatedTile = RotatedAndFlipped(tile, border.rotations, border.flipped)
//            if (rotatedTile.contents.map(_.apply(0)).mkString("") == toMatchL)
//              Some(tile -> border)
//            else
//              None
//        }).head
//        imageWithTiles(y)(x) = RotatedAndFlipped(selectedTile._1, selectedTile._2.rotations, selectedTile._2.flipped)
//        tilesLeft.remove(selectedTile._1)
      }
    }
    imageWithTiles
  }.toOption

  (for {
    rotated <- 0 until 4
    flipped <- List(true, false)
  } yield {
    trySolution(rotated, flipped)
  }).flatten.map { solution =>
    println("New solution:")
    solution(0).foreach{ tile =>
      println()
      tile.contents.map(row => println(row.mkString("")))
    }
  }


}
