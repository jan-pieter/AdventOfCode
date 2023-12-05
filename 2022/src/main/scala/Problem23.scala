import scala.io.Source

object Problem23 extends App:
  val input = Source.fromResource("23-input.txt").getLines().toVector

  case class Position(x: Long, y: Long) {
    def surrounding: Vector[Position] = (for {
      xDiff <- -1 to 1
      yDiff <- -1 to 1
      if xDiff != 0 && yDiff != 0
    } yield Position(x + xDiff, y + yDiff)).toVector
    def adjacent(direction: Direction): Vector[Position] = (direction match {
      case Direction.N => for { xDiff <- -1 to 1 } yield Position(x + xDiff, y - 1)
      case Direction.S => for { xDiff <- -1 to 1 } yield Position(x + xDiff, y + 1)
      case Direction.E => for { yDiff <- -1 to 1 } yield Position(x + 1, y + yDiff)
      case Direction.W => for { yDiff <- -1 to 1 } yield Position(x - 1, y + yDiff)
    }).toVector
    def next(direction: Direction): Position = direction match {
      case Direction.N => copy(y = y - 1)
      case Direction.S => copy(y = y + 1)
      case Direction.E => copy(x = x + 1)
      case Direction.W => copy(x = x - 1)
    }
  }
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
      if (elf.position.surrounding.exists(byPos.contains)) {
        elf.copy(
          nextPosition = directions
            .flatMap(direction => Option.when(!elf.position.adjacent(direction).exists(byPos.contains))(elf.position.next(direction)))
            .headOption
            .getOrElse(elf.position)
        )
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