import Problem18.Direction.*

import scala.collection.mutable
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

//  val depth = input.filter(_.direction == South).map(_.length).sum * 2 + 5
//  val width = input.filter(_.direction == East).map(_.length).sum * 2 + 5
//  val startY = depth / 2
//  val startX = width / 2
  val startY = 0L
  val startX = 0L
  println(s"$startY $startX")
//  val ground = Array.fill(depth, width)('.')
//  ground(startY)(startX) = '#'
  var loop = Set((startY, startX))
  case class Line(y1: Long, x1: Long, y2: Long, x2: Long)
  var lines = Set.empty[Line]
  input.foldLeft((startY, startX)) { (pos, dig) =>
    val endPos = dig.direction match {
      case North => (pos._1 - dig.length, pos._2)
      case South => (pos._1 + dig.length, pos._2)
      case West => (pos._1, pos._2 - dig.length)
      case East => (pos._1, pos._2 + dig.length)
    }
    /*val positions = (1 to dig.length).map(i => dig.direction match {
      case North => (pos._1 - i, pos._2)
      case South => (pos._1 + i, pos._2)
      case West => (pos._1, pos._2 - i)
      case East => (pos._1, pos._2 + i)
    })
    positions.foreach { pos2 =>
      loop = loop + pos2
//      ground(pos2._1)(pos2._2) = '#'
    }*/
    lines = lines + Line(pos._1, pos._2, endPos._1, endPos._2)
    if endPos == (startY, startX) then println("At START")
    endPos
  }

//  ground.foreach(line => println(line.mkString))

//  ground.slice(startY - 5, startY - 5 + 10).transpose.slice(startX - 5, startX - 5 + 10).transpose.foreach(line => println(line.mkString))

/*  val queue = mutable.Queue((0,0))
  while (queue.nonEmpty) {
    val pos = queue.dequeue()
    if (pos._1 >= 0 && pos._1 < ground.length && pos._2 >= 0 && pos._2 < ground.head.length) {
      if (ground(pos._1)(pos._2) != 'X' && !loop(pos)) {
        ground(pos._1)(pos._2) = 'X'
        queue.addOne(pos._1 - 1 -> pos._2)
        queue.addOne(pos._1 + 1 -> pos._2)
        queue.addOne(pos._1 -> (pos._2 - 1))
        queue.addOne(pos._1 -> (pos._2 + 1))
      }
    }
  }*/

//  println(ground.map(_.count(_ != 'X')).sum)

  def pointsAt(y: Long): Set[Long] =
    lines.filter(line => (y >= line.y1 && y <= line.y2) || (y <= line.y1 && y >= line.y2)).flatMap(line =>
      if line.y1 == line.y2 then
        Range.Long.inclusive(line.x1, line.x2, if line.x1 < line.x2 then 1 else -1).toSet
      else
        Set(line.x1)
    )
  val minY = lines.map(_.y1).min
  val maxY = lines.map(_.y1).max
//  val endLines = lines.map(_.y1).toVector.sorted
//  val points2: Map[Long, Set[Line]] = endLines.map(y => y -> lines.filter(line => (y >= line.y1 && y <= line.y2) || (y <= line.y1 && y >= line.y2)))
//  val points: Map[Int, Set[Int]] = loop.groupBy(_._1).map((key, values) => key -> values.map(_._2)).withDefaultValue(Set.empty[Int])
//  val pointsV = points.toVector.sortBy(_._1).map((key, values) => key -> values.toVector.sorted)
//  pointsV.foreach((key, values) =>  println(s"$key => $values"))

  val result = Range.Long.inclusive(minY, maxY, 1).map { y =>
    val prevPoints = pointsAt(y-1)
    val linePoints = pointsAt(y)
    val xs = linePoints.toVector.sorted
//    print(s"$y: $xs, ")
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
  println()

/*  val result = pointsV.map { (y, xs) =>
//    println(s"Line $y")
    xs.zipWithIndex.foldLeft((false, 0L, North)){
      case ((inside, count, prevDir), (x, index)) if points(y).contains(x-1) && !points(y).contains(x+1) =>
        val direction = if points(y-1).contains(x) then North else South
//        println(s"Case 1: $inside, $count, $prevDir, $x, $index $direction")
        (if direction == prevDir then inside else !inside, count + 1, prevDir)
      case ((inside, count, prevDir), (x, index)) if points(y).contains(x - 1) =>
//        println(s"Case 2: $inside, $count, $prevDir, $x, $index")
        (inside, count + 1, prevDir)
      case ((false, count, prevDir), (x, index)) if !points(y).contains(x+1) =>
//        println(s"Case 3: false, $count, $prevDir, $x, $index")
        (true, count+1, if points(y-1).contains(x) then North else South)
      case ((false, count, prevDir), (x, index)) =>
//        println(s"Case 4: false, $count, $prevDir, $x, $index")
        (false, count + 1, if points(y - 1).contains(x) then North else South)
      case ((true, count, prevDir), (x, index)) if points(y).contains(x+1) =>
//        println(s"Case 5: true, $count, $prevDir, $x, $index")
        (true, count + (x - xs(index - 1)), if points(y-1).contains(x) then North else South)
      case ((true, count, prevDir), (x, index)) =>
//        println(s"Case 6: false, $count, $prevDir, $x, $index")
        (false, count + (x - xs(index-1)), prevDir)
    }._2
  }*/
//  result.foreach(println(_))
  println(result.sum)












