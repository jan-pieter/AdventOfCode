import java.text.NumberFormat.Field
import scala.io.Source

object Problem23 extends App:
  val input = Source.fromResource("23-input.txt").getLines().toVector

  case class Position(x: Long, y: Long)
  case class Elf(position: Position, nextPosition: Position)
  enum Direction:
    case N, E, S, W

  val allDirections = Vector(Direction.N, Direction.S, Direction.W, Direction.E)

  val elves = (for {
    y <- input.indices
    x <- input(y).indices
  } yield input(y)(x) match {
    case '#' => Some(Elf(Position(x, y), Position(x, y)))
    case '.' => None
  }).flatten.toVector

  def printWorld(elves: Vector[Elf]): Unit = {
    val minX = elves.map(_.position.x).min
    val maxX = elves.map(_.position.x).max
    val minY = elves.map(_.position.y).min
    val maxY = elves.map(_.position.y).max
    val array = Array.fill((maxY-minY).toInt + 1, (maxX-minX).toInt + 1)('.')
    elves.foreach(elf => array((elf.position.y + (0-minY)).toInt)((elf.position.x + (0-minX)).toInt) = '#')
    array.foreach(line => println(line.mkString))
  }

  def doRound(i: Int, elves: Vector[Elf]): Vector[Elf] = {
    val directions = allDirections.splitAt(i % 4) match { case (first, second) => second ++ first }
//    println(s"Round $i $directions")
    val byPos = elves.groupBy(_.position)
    val elvesWithPos = elves.map { elf =>
      if (byPos.contains(Position(elf.position.x - 1, elf.position.y - 1)) || byPos.contains(Position(elf.position.x, elf.position.y - 1)) ||
        byPos.contains(Position(elf.position.x + 1, elf.position.y - 1)) || byPos.contains(Position(elf.position.x - 1, elf.position.y)) ||
        byPos.contains(Position(elf.position.x + 1, elf.position.y)) || byPos.contains(Position(elf.position.x - 1, elf.position.y + 1)) ||
        byPos.contains(Position(elf.position.x, elf.position.y + 1)) || byPos.contains(Position(elf.position.x + 1, elf.position.y + 1))
      ) {
        elf.copy(nextPosition = directions.flatMap {
          case Direction.N => Option.when(
            !byPos.contains(Position(elf.position.x - 1, elf.position.y - 1)) &&
              !byPos.contains(Position(elf.position.x, elf.position.y - 1)) &&
              !byPos.contains(Position(elf.position.x + 1, elf.position.y - 1))
          )(elf.position.copy(y = elf.position.y - 1))
          case Direction.S => Option.when(
            !byPos.contains(Position(elf.position.x - 1, elf.position.y + 1)) &&
              !byPos.contains(Position(elf.position.x, elf.position.y + 1)) &&
              !byPos.contains(Position(elf.position.x + 1, elf.position.y + 1))
          )(elf.position.copy(y = elf.position.y + 1))
          case Direction.E => Option.when(
            !byPos.contains(Position(elf.position.x + 1, elf.position.y - 1)) &&
              !byPos.contains(Position(elf.position.x + 1, elf.position.y)) &&
              !byPos.contains(Position(elf.position.x + 1, elf.position.y + 1))
          )(elf.position.copy(x = elf.position.x + 1))
          case Direction.W => Option.when(
            !byPos.contains(Position(elf.position.x - 1, elf.position.y - 1)) &&
              !byPos.contains(Position(elf.position.x - 1, elf.position.y)) &&
              !byPos.contains(Position(elf.position.x - 1, elf.position.y + 1))
          )(elf.position.copy(x = elf.position.x - 1))
        }.headOption.getOrElse(elf.position))
      } else elf.copy(nextPosition = elf.position)
    }
    val byNextPos = elvesWithPos.groupBy(_.nextPosition)
    val result = elvesWithPos.map { elf =>
      if (byNextPos(elf.nextPosition).length == 1) elf.copy(position = elf.nextPosition) else elf
    }
//    printWorld(result)
    result
  }

  val start = System.nanoTime()
  val end = (0 until 10).foldLeft(elves)((elves, round) => doRound(round, elves))
  val minX = end.map(_.position.x).min
  val maxX = end.map(_.position.x).max
  val minY = end.map(_.position.y).min
  val maxY = end.map(_.position.y).max
  val answer = (maxX - minX + 1) * (maxY - minY + 1) - end.length
  println(answer)
  val end1 = System.nanoTime()
  println(s"Part 1 took ${(end1 - start)/1000000} ms")

  var round = 0
  var done = false
  var world = elves
  while (!done) {
    val oldWorld = world
    world = doRound(round, world)
    round += 1
    done = (oldWorld == world)
  }
  println(round)
  val end2 = System.nanoTime()
  println(s"Part 2 took ${(end2 - start)/1000000} ms")