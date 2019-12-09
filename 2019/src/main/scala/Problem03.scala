import scala.io.Source
import Math.{abs, min, max}

object Problem03 extends App {

  // 188 + 401 = 589
  // Y1: -30 Y2: 53 X: 158 Up
  // X1: 155 X2: 238 Y: -12 Right
  // 30-12: 18
  // 158-155 = 3
  // Total 589 + 18 + 3 = 610
  val lines = Source.fromResource("03-input.txt").getLines().take(2).toVector

//  val lines = Vector("R8,U5,L5,D3", "U7,R6,D4,L4")
//  val lines = Vector("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83")
//  val lines = Vector("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")

  sealed trait Segment
  case class HorizontalSegment(x1: Int, x2: Int, y: Int, length: Int, direction: String) extends Segment
  case class VerticalSegment(y1: Int, y2: Int, x: Int, length: Int, direction: String) extends Segment

  def toSegments(string: String): List[Segment] = {
    case class State(segments: List[Segment], currentX: Int, currentY: Int, length: Int)
    def length(string: String) = string.drop(1).toInt
    val state = string.split(",").foldLeft(State(Nil, 0, 0, 0)){ (state, segmentString) => segmentString match {
      case str if str.startsWith("D") =>
        val endY = state.currentY - length(str)
        val l = state.length + length(str)
        State(VerticalSegment(min(state.currentY, endY), max(state.currentY, endY), state.currentX, state.length, "D") :: state.segments, state.currentX, endY, l)
      case str if str.startsWith("U") =>
        val endY = state.currentY + length(str)
        val l = state.length + length(str)
        State(VerticalSegment(min(state.currentY, endY), max(state.currentY, endY), state.currentX, state.length, "U") :: state.segments, state.currentX, endY, l)
      case str if str.startsWith("L") =>
        val endX = state.currentX - length(str)
        val l = state.length + length(str)
        State(HorizontalSegment(min(state.currentX, endX), max(state.currentX, endX), state.currentY, state.length, "L") :: state.segments, endX, state.currentY, l)
      case str if str.startsWith("R") =>
        val endX = state.currentX + length(str)
        val l = state.length + length(str)
        State(HorizontalSegment(min(state.currentX, endX), max(state.currentX, endX), state.currentY, state.length, "R") :: state.segments, endX, state.currentY, l)
    }}
    state.segments.reverse
  }

  def crossDistance(segment1: Segment, segment2: Segment): Option[Int] = (segment1, segment2) match {
    case (_: HorizontalSegment, _: HorizontalSegment) => None
    case (_: VerticalSegment, _: VerticalSegment) => None
    case (segment1: HorizontalSegment, segment2: VerticalSegment) =>
      if (segment1.y > segment2.y1 && segment1.y < segment2.y2 && segment2.x > segment1.x1 && segment2.x < segment1.x2) {
        Some(abs(segment1.y) + abs(segment2.x))
      } else {
        None
      }
    case (segment1: VerticalSegment, segment2: HorizontalSegment) =>
      if (segment1.x > segment2.x1 && segment1.x < segment2.x2 && segment2.y > segment1.y1 && segment2.y < segment1.y2) {
        Some(abs(segment1.x) + abs(segment2.y))
      } else {
        None
      }
  }

  def crossLength(segment1: Segment, segment2: Segment): Option[Int] = (segment1, segment2) match {
    case (_: HorizontalSegment, _: HorizontalSegment) => None
    case (_: VerticalSegment, _: VerticalSegment) => None
    case (segment1: HorizontalSegment, segment2: VerticalSegment) =>
      if (segment1.y > segment2.y1 && segment1.y < segment2.y2 && segment2.x > segment1.x1 && segment2.x < segment1.x2) {
        val segment1L = segment1.direction match {
          case "L" => abs(segment1.x2 - segment2.x)
          case "R" => abs(segment1.x1 - segment2.x)
        }
        val segment2L = segment2.direction match {
          case "U" => abs(segment2.y1 - segment1.y)
          case "D" => abs(segment2.y2 - segment1.y)
        }
        val l = segment1.length + segment2.length + segment2L + segment1L
        println(s"Intersection at ${segment2.x} ${segment1.y} between $segment1 and $segment2 with length $l")
        Some(l)
      } else {
        None
      }
    case (segment1: VerticalSegment, segment2: HorizontalSegment) =>
      val segment2L = segment2.direction match {
        case "L" => abs(segment2.x2 - segment1.x)
        case "R" => abs(segment2.x1 - segment1.x)
      }
      val segment1L = segment1.direction match {
        case "U" => abs(segment1.y1 - segment2.y)
        case "D" => abs(segment1.y2 - segment2.y)
      }
      if (segment1.x > segment2.x1 && segment1.x < segment2.x2 && segment2.y > segment1.y1 && segment2.y < segment1.y2) {
        val l = segment1.length + segment2.length + segment1L + segment2L
        println(s"Intersection at ${segment1.x} ${segment2.y} between $segment1 and $segment2 with length $l segment1L $segment1L segment2L $segment2L")
        Some(l)
      } else {
        None
      }
  }

  val wire1 = toSegments(lines(0))
  val wire2 = toSegments(lines(1))

  println(wire2.flatMap(segment2 => wire1.flatMap(segment1 => crossDistance(segment1, segment2))).min)
  println(wire2.flatMap(segment2 => wire1.flatMap(segment1 => crossLength(segment1, segment2))).min)

}
