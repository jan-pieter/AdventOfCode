import scala.io.Source

object Problem12 extends App {

  val input = Source.fromResource("12-input.txt").getLines().toList

  case class State(x: Int, y: Int, direction: Int) {
    def distance: Int = x.abs + y.abs
  }

  val end = input.map(str => str.head -> str.tail.toInt).foldLeft(State(0, 0, 0)) {
    case (state, ('N', distance)) => state.copy(y = state.y - distance)
    case (state, ('S', distance)) => state.copy(y = state.y + distance)
    case (state, ('E', distance)) => state.copy(x = state.x + distance)
    case (state, ('W', distance)) => state.copy(x = state.x - distance)
    case (state, ('L', degrees)) => state.copy(direction = (state.direction - degrees) % 360)
    case (state, ('R', degrees)) => state.copy(direction = (state.direction + degrees) % 360)
    case (state, ('F', distance)) => (state.direction + 360) % 360 match {
      case 0 => state.copy(x = state.x + distance)
      case 90 => state.copy(y = state.y + distance)
      case 180 => state.copy(x = state.x - distance)
      case 270 => state.copy(y = state.y - distance)
    }
  }

  println(end.distance)

  case class StateWithWaypoint(x: Int, y: Int, waypointX: Int, waypointY: Int) {
    def distance: Int = x.abs + y.abs
    def rotateLeft: StateWithWaypoint = this.copy(waypointX = waypointY, waypointY = -1*waypointX)
    def rotateRight: StateWithWaypoint = this.copy(waypointY = waypointX, waypointX = -1*waypointY)
  }

  val endWithWaypoint = input.map(str => str.head -> str.tail.toInt).foldLeft(StateWithWaypoint(0, 0, 10, -1)) {
    case (state, ('N', distance)) => val result = state.copy(waypointY = state.waypointY - distance); println(result); result
    case (state, ('S', distance)) => val result = state.copy(waypointY = state.waypointY + distance); println(result); result
    case (state, ('E', distance)) => val result = state.copy(waypointX = state.waypointX + distance); println(result); result
    case (state, ('W', distance)) => val result = state.copy(waypointX = state.waypointX - distance); println(result); result
    case (state, ('L', degrees)) => val result = (0 until (degrees / 90)).foldLeft(state) { case (state, _) => state.rotateLeft }; println(result); result
    case (state, ('R', degrees)) => val result = (0 until (degrees / 90)).foldLeft(state) { case (state, _) => state.rotateRight }; println(result); result
    case (state, ('F', distance)) => val result = state.copy(x = state.x + distance * state.waypointX, y = state.y + distance * state.waypointY); println(result); result
  }

  println(endWithWaypoint.distance)

}
