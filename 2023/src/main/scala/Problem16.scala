import Problem16.Direction.*

import scala.annotation.tailrec
import scala.io.Source

object Problem16 extends App:
  val input = Source.fromResource("16-input.txt").getLines().toVector

  enum Direction:
    case North, East, South, West

  case class Beam(x: Int, y: Int, direction: Direction) {
    val isValid: Boolean = x >= 0 && y >= 0 && y < input.length && x < input.head.length
    def mirror: Char = input(y)(x)
    def continue: Beam = direction match
      case North => copy(y = y - 1)
      case East => copy(x = x + 1)
      case South => copy(y = y + 1)
      case West => copy(x = x - 1)
  }

  @tailrec
  def beams(active: Vector[Beam], seen: Set[Beam]): Set[Beam] = {
    val notSeen = active.filterNot(seen)
    if notSeen.isEmpty then seen else
      val beam = notSeen.head
      val nextDirection = (beam.mirror, beam.direction) match {
        case ('.', direction) => Vector(direction)
        case ('|', North) => Vector(North)
        case ('|', South) => Vector(South)
        case ('-', East) => Vector(East)
        case ('-', West) => Vector(West)
        case ('|', _) => Vector(North, South)
        case ('-', _) => Vector(West, East)
        case ('/', North) => Vector(East)
        case ('/', East) => Vector(North)
        case ('/', South) => Vector(West)
        case ('/', West) => Vector(South)
        case ('\\', North) => Vector(West)
        case ('\\', East) => Vector(South)
        case ('\\', South) => Vector(East)
        case ('\\', West) => Vector(North)
      }
      val next = nextDirection.map(direction => beam.copy(direction = direction).continue)
      beams(notSeen.tail ++ next.filter(_.isValid), seen + beam)
  }

  val result = beams(Vector(Beam(0, 0, East)), Set.empty[Beam])
  println(result.map(beam => beam.x -> beam.y).size)

  val starts =
    input.indices.flatMap(y => Vector(Beam(0, y, East), Beam(input.head.length-1, y, West))) ++
      input.head.indices.flatMap(x => Vector(Beam(x, 0, South), Beam(x, input.length-1, North)))

  println(starts.map(start => beams(Vector(start), Set.empty).map(beam => beam.x -> beam.y).size).max)