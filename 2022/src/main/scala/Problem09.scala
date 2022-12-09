import scala.io.Source

object Problem09 extends App:
  val input = Source.fromResource("09-input.txt").getLines().toVector.flatMap {
    case s"$direction $count" => Vector.fill(count.toInt)(direction)
  }

  case class State(knots: Vector[(Int, Int)], tailVisited: Set[(Int, Int)]) {
    def move(direction: String): State = knots.indices.foldLeft(this)((state, index) => state.moveKnot(index, direction))

    def moveHead(direction: String): State = direction match {
      case "U" => copy(knots.updated(0, knots(0)._1 -> (knots(0)._2 + 1)))
      case "D" => copy(knots.updated(0, knots(0)._1 -> (knots(0)._2 - 1)))
      case "L" => copy(knots.updated(0, (knots(0)._1 - 1) -> knots(0)._2))
      case "R" => copy(knots.updated(0, (knots(0)._1 + 1) -> knots(0)._2))
    }

    def shouldMoveKnot(index: Int): Boolean = Math.abs(knots(index)._1 - knots(index - 1)._1) > 1 || Math.abs(knots(index)._2 - knots(index - 1)._2) > 1

    def moveKnot(index: Int, direction: String): State = {
      if (index == 0) moveHead(direction)
      else if (shouldMoveKnot(index)) {
        val xMove = if (knots(index - 1)._1 - knots(index)._1 > 0) 1 else if (knots(index - 1)._1 == knots(index)._1) 0 else -1
        val yMove = if (knots(index - 1)._2 - knots(index)._2 > 0) 1 else if (knots(index - 1)._2 == knots(index)._2) 0 else -1
        val newKnotPos = (knots(index)._1 + xMove) -> (knots(index)._2 + yMove)
        val newTailVisited = if (index == knots.length - 1) tailVisited + newKnotPos else tailVisited
        copy(knots.updated(index, newKnotPos), newTailVisited)
      } else this
    }
  }

  val endState = input.foldLeft(State(Vector.fill(2)(0 -> 0), Set(0 -> 0)))((state, direction) => state.move(direction))
  println(endState.tailVisited.size)

  val endState2 = input.foldLeft(State(Vector.fill(10)(0 -> 0), Set(0 -> 0)))((state, direction) => state.move(direction))
  println(endState2.tailVisited.size)
