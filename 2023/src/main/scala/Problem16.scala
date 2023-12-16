import Problem16.Direction.*
import scala.io.Source

object Problem16 extends App:
  val input = Source.fromResource("16-input.txt").getLines().toVector

  enum Direction:
    case North, East, South, West

  case class Beam(x: Int, y: Int, direction: Direction) {
    def mirror: Char = input(y)(x)
    def continue: Beam = direction match {
      case North => copy(y = y - 1)
      case East => copy(x = x + 1)
      case South => copy(y = y + 1)
      case West => copy(x = x - 1)
    }
    val isValid: Boolean = x >= 0 && y >= 0 && y < input.length && x < input.head.length
  }

  def beams(active: Vector[Beam], seen: Set[Beam]): Set[Beam] = {
    val notSeen = active.filterNot(seen)
    if notSeen.isEmpty then seen else
      val beam = notSeen.head
      val next: Vector[Beam] = (beam.mirror, beam.direction) match {
        case ('.', _) => Vector(beam.continue)
        case ('|', North) | ('|', South) => Vector(beam.continue)
        case ('-', East) | ('-', West) => Vector(beam.continue)
        case ('|', _) => Vector(Beam(beam.x, beam.y - 1, North), Beam(beam.x, beam.y + 1, South))
        case ('-', _) => Vector(Beam(beam.x - 1, beam.y, West), Beam(beam.x + 1, beam.y, East))
        case ('/', North) => Vector(Beam(beam.x + 1, beam.y, East))
        case ('/', East) => Vector(Beam(beam.x, beam.y - 1, North))
        case ('/', South) => Vector(Beam(beam.x - 1, beam.y, West))
        case ('/', West) => Vector(Beam(beam.x, beam.y + 1, South))
        case ('\\', North) => Vector(Beam(beam.x - 1, beam.y, West))
        case ('\\', East) => Vector(Beam(beam.x, beam.y + 1, South))
        case ('\\', South) => Vector(Beam(beam.x + 1, beam.y, East))
        case ('\\', West) => Vector(Beam(beam.x, beam.y - 1, North))
      }
      beams(notSeen.tail ++ next.filter(_.isValid), seen + beam)
  }

  val result = beams(Vector(Beam(0, 0, East)), Set.empty[Beam])
  println(result.map(beam => beam.x -> beam.y).size)

  val starts =
    input.indices.flatMap(y => Vector(Beam(0, y, East), Beam(input.head.length-1, y, West))) ++
      input.head.indices.flatMap(x => Vector(Beam(x, 0, South), Beam(x, input.length-1, North)))

  println(starts.map(start => beams(Vector(start), Set.empty).map(beam => beam.x -> beam.y).size).max)