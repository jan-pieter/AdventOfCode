import scala.io.Source

object Problem02 extends App {

  val input: Vector[(String, Int)] = Source.fromResource("02-input.txt").getLines().map { line =>
    val splitted = line.split(" ")
    splitted(0) -> splitted(1).toInt
  }.toVector

  case class State(horizontal: Int, vertical: Int, aim: Int)

  val endState = input.foldLeft(State(0,0,0)) {
    case (state, ("forward", x)) => state.copy(horizontal = state.horizontal + x)
    case (state, ("down", x)) => state.copy(vertical = state.vertical + x)
    case (state, ("up", x)) => state.copy(vertical = state.vertical - x)
  }

  println(s"State $endState : ${endState.vertical * endState.horizontal}")

  val endState2 = input.foldLeft(State(0,0,0)) {
    case (state, ("forward", x)) => state.copy(horizontal = state.horizontal + x, vertical = state.vertical + x * state.aim)
    case (state, ("down", x)) => state.copy(aim = state.aim + x)
    case (state, ("up", x)) => state.copy(aim = state.aim - x)
  }

  println(s"State $endState2 : ${endState2.vertical * endState2.horizontal}")
}
