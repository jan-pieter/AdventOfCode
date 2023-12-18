import Problem18.Direction.*

import scala.io.Source

object Problem18 extends App:
  enum Direction:
    case North, East, South, West
  case class Dig(direction: Direction, length: Long, color: String) {
    def swap: Dig = {
      val direction = color.last match {
        case '0' => East
        case '1' => South
        case '2' => West
        case '3' => North
      }
      Dig(direction, java.lang.Long.parseLong(color.dropRight(1), 16), "")
    }
  }
  val input = Source.fromResource("18-input.txt").getLines().toVector.map {
    case s"$directionStr $length (#$rgb)" =>
      val direction = directionStr match {
        case "R" => East
        case "D" => South
        case "L" => West
        case "U" => North
      }
      Dig(direction, length.toLong, rgb).swap
  }

  case class Line(y1: Long, x1: Long, y2: Long, x2: Long)
  var lines = Set.empty[Line]
  input.foldLeft((0L, 0L)) { (pos, dig) =>
    val endPos = dig.direction match {
      case North => (pos._1 - dig.length, pos._2)
      case South => (pos._1 + dig.length, pos._2)
      case West => (pos._1, pos._2 - dig.length)
      case East => (pos._1, pos._2 + dig.length)
    }
    lines = lines + Line(pos._1, pos._2, endPos._1, endPos._2)
    if endPos == (0L, 0L) then println("At START")
    endPos
  }

  //TODO: Do it without listing all points/lines
  def pointsAt(y: Long): Set[Long] =
    lines.filter(line => (y >= line.y1 && y <= line.y2) || (y <= line.y1 && y >= line.y2)).flatMap(line =>
      if line.y1 == line.y2 then
        Range.Long.inclusive(line.x1, line.x2, if line.x1 < line.x2 then 1 else -1).toSet
      else
        Set(line.x1)
    )
  val minY = lines.map(_.y1).min
  val maxY = lines.map(_.y1).max

  val result = Range.Long.inclusive(minY, maxY, 1).map { y =>
    val prevPoints = pointsAt(y-1)
    val linePoints = pointsAt(y)
    val xs = linePoints.toVector.sorted
    xs.zipWithIndex.foldLeft((false, 0L, North)) {
      case ((inside, count, prevDir), (x, index)) if linePoints.contains(x - 1) && !linePoints.contains(x + 1) =>
        val direction = if prevPoints.contains(x) then North else South
        //        println(s"Case 1: $inside, $count, $prevDir, $x, $index $direction")
        (if direction == prevDir then inside else !inside, count + 1, prevDir)
      case ((inside, count, prevDir), (x, index)) if linePoints.contains(x - 1) =>
        //        println(s"Case 2: $inside, $count, $prevDir, $x, $index")
        (inside, count + 1, prevDir)
      case ((false, count, prevDir), (x, index)) if !linePoints.contains(x + 1) =>
        //        println(s"Case 3: false, $count, $prevDir, $x, $index")
        (true, count + 1, if prevPoints.contains(x) then North else South)
      case ((false, count, prevDir), (x, index)) =>
        //        println(s"Case 4: false, $count, $prevDir, $x, $index")
        (false, count + 1, if prevPoints.contains(x) then North else South)
      case ((true, count, prevDir), (x, index)) if linePoints.contains(x + 1) =>
        //        println(s"Case 5: true, $count, $prevDir, $x, $index")
        (true, count + (x - xs(index - 1)), if prevPoints.contains(x) then North else South)
      case ((true, count, prevDir), (x, index)) =>
        //        println(s"Case 6: false, $count, $prevDir, $x, $index")
        (false, count + (x - xs(index - 1)), prevDir)
    }._2
  }
//  println()
//  result.foreach(println(_))
  println(result.sum)












