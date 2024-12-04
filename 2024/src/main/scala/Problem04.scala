import scala.annotation.tailrec
import scala.io.Source

object Problem04 extends App:
//  val file = "04-test.txt"
  val file = "04-input.txt"
  val input = Source.fromResource(file).getLines().toVector

  enum Direction:
    case Up, Down, Left, Right, UpLeft, UpRight, DownLeft, DownRight

  @tailrec
  def isMatch(y: Int, x: Int, direction: Direction, toMatch: String): Boolean =
    input(y)(x) == toMatch.head && {
      val (dy, dx) = direction match
        case Direction.Up => (-1, 0)
        case Direction.Down => (1, 0)
        case Direction.Left => (0, -1)
        case Direction.Right => (0, 1)
        case Direction.UpLeft => (-1, -1)
        case Direction.UpRight => (-1, 1)
        case Direction.DownLeft => (1, -1)
        case Direction.DownRight => (1, 1)
      val (ny, nx) = (y + dy, x + dx)

      if toMatch.tail.isEmpty then true
      else if ny < 0 || ny >= input.length || nx < 0 || nx >= input(ny).length then false
      else isMatch(ny, nx, direction, toMatch.tail)
    }

  val result = (for {
    y <- input.indices
    x <- input(y).indices
    direction <- Direction.values
  } yield isMatch(y, x, direction, "XMAS")).count(identity)
  println(result)

  val result2 = (for {
    y <- input.indices
    x <- input(y).indices
    direction <- List(Direction.UpLeft, Direction.UpRight, Direction.DownLeft, Direction.DownRight)
  } yield isMatch(y, x, direction, "MAS") && { direction match {
    case Direction.UpLeft => isMatch(y, x - 2, Direction.UpRight, "MAS") || isMatch(y - 2, x, Direction.DownLeft, "MAS")
    case Direction.UpRight => isMatch(y, x + 2, Direction.UpLeft, "MAS") || isMatch(y - 2, x, Direction.DownRight, "MAS")
    case Direction.DownLeft => isMatch(y, x - 2, Direction.DownRight, "MAS") || isMatch(y + 2, x, Direction.UpLeft, "MAS")
    case Direction.DownRight => isMatch(y, x + 2, Direction.DownLeft, "MAS") || isMatch(y + 2, x, Direction.UpRight, "MAS")
  }}).count(identity)

  println(result2 / 2)