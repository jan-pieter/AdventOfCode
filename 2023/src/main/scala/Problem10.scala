import Problem10.Direction.*

import scala.io.Source

object Problem10 extends App:
  val input = Source.fromResource("10-input.txt").getLines().toVector
  val startY = input.indexWhere(_.indexOf("S") != -1)
  val startX = input(startY).indexOf("S")

  enum Direction(val y: Int, val x: Int):
    case North extends Direction(-1, 0)
    case East extends Direction(0, 1)
    case South extends Direction(1, 0)
    case West extends Direction(0, -1)

  def next(pipe: Char, origin: Direction): Option[Direction] = (pipe, origin) match {
    case ('|', North) => Some(North)
    case ('|', South) => Some(South)
    case ('-', East) => Some(East)
    case ('-', West) => Some(West)
    case ('7', East) => Some(South)
    case ('7', North) => Some(West)
    case ('F', West) => Some(South)
    case ('F', North) => Some(East)
    case ('L', West) => Some(North)
    case ('L', South) => Some(East)
    case ('J', South) => Some(West)
    case ('J', East) => Some(North)
    case ('S', dir) => Some(dir)
    case _ => None
  }

  // TODO: Check if we can make this tail recursive so that we don't need a large stack...
  def step(pos: (Int, Int), length: Long, direction: Direction): Option[Set[(Int, Int)]] = {
    if pos._1 >= 0 && pos._1 < input.length && pos._2 >= 0 && pos._2 < input.head.length then
      if input(pos._1)(pos._2) == 'S' && length > 0L then Some(Set.empty) else
        val n = next(input(pos._1)(pos._2), direction)
        n.flatMap(nextDirection => step((pos._1 + nextDirection.y, pos._2 + nextDirection.x), length + 1, nextDirection).map(_ + pos))
    else
      None
  }

  val allDirections = Vector(
    step((startY, startX), 0L, North),
    step((startY, startX), 0L, East),
    step((startY, startX), 0L, South),
    step((startY, startX), 0L, West)
  )

  val loop = allDirections.flatten.head
//  println(loop)
  println(loop.size / 2)

  val toEast = Set('S', '-', 'L', 'F')
  val toWest = Set('S', '-', 'J', '7')
  val toNorth = Set('S', '|', 'J', 'L')
  val toSouth = Set('S', '|', 'F', '7')

  def print(arr: Array[Array[Char]]): Unit = {
    arr.foreach(line => println(line.mkString))
  }

  // Embedded
  val embedded = Array.fill(input.length+2, input.head.length+2)('.')
  for {
    y <- input.indices
    x <- input.head.indices
  } yield {
    embedded(y+1)(x+1) = input(y)(x)
  }
  val embeddedLoop = loop.map((y, x) => (y+1, x+1))
//  println()
//  println("Embedded")
//  print(embedded)
//  println(embeddedLoop)

  // Expand
  val expanded = Array.fill(embedded.length*2, embedded.head.length*2)('?')
  var expandedLoop = embeddedLoop.map((y, x) => (y*2) -> (x*2))
  for {
    y <- expanded.indices
    x <- expanded.head.indices
  } yield {
    if (y % 2 == 0 && x % 2 == 0) {
      expanded(y)(x) = embedded(y / 2)(x / 2)
    } else if (y == expanded.length - 1 || x == expanded.head.length - 1) {
      expanded(y)(x) = '.'
    } else if (y % 2 == 1 && x % 2 == 1) {
      expanded(y)(x) = '.'
    } else if (x % 2 == 1) {
      if (!embeddedLoop.contains(y / 2 -> x / 2) || !embeddedLoop.contains(y / 2 -> ((x/2)+1))) {
        expanded(y)(x) = '.'
      } else if (toEast(embedded(y/2)(x/2)) && toWest(embedded(y/2)((x/2)+1))) {
        expanded(y)(x) = '-'
        expandedLoop = expandedLoop + (y -> x)
      } else {
        expanded(y)(x) = '.'
      }
    } else {
      if (!embeddedLoop.contains(y / 2 -> x / 2) || !embeddedLoop.contains(((y / 2)+1) -> x / 2)) {
        expanded(y)(x) = '.'
      } else if (toSouth(embedded(y/2)(x/2)) && toNorth(embedded((y/2)+1)(x/2))) {
        expanded(y)(x) = '|'
        expandedLoop = expandedLoop + (y -> x)
      } else {
        expanded(y)(x) = '.'
      }
    }
  }

//  println()
//  println("Expanded")
//  print(expanded)
//  println(expandedLoop)

  // Flood fill outside
  def flood(pos: (Int, Int)): Unit = {
    if (pos._1 >= 0 && pos._1 < expanded.length && pos._2 >= 0 && pos._2 < expanded.head.length) {
      if (expanded(pos._1)(pos._2) != '#' && !expandedLoop(pos)) {
        expanded(pos._1)(pos._2) = '#'
        flood(pos._1-1 -> pos._2)
        flood(pos._1+1 -> pos._2)
        flood(pos._1 -> (pos._2-1))
        flood(pos._1 -> (pos._2+1))
      }
    }
  }
  flood((0,0))


//  println()
//  println("Filled")
//  print(expanded)

  // Remove even rows & columns
  val original = expanded.zipWithIndex.filter(_._2 % 2 == 0).map(_._1.zipWithIndex.filter(_._2 % 2 == 0).map(_._1))

//  println()
//  println("Original")
//  print(original)
  // Answer = total - filled - pipe length
  val answer = original.length * original.head.length - original.map(_.count(_ == '#')).sum - loop.size

  println(answer)