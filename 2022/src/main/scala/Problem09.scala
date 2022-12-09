import scala.io.Source

object Problem09 extends App:
  val input = Source.fromResource("09-input.txt").getLines().toVector.flatMap {
    case s"$direction $count" => Vector.fill(count.toInt)(direction)
  }

  case class State(headPos: (Int, Int), tailPos: (Int, Int), tailVisited: Set[(Int, Int)]) {
    def move(direction: String): State = moveHead(direction).moveTail
    def moveHead(direction: String): State = direction match {
      case "U" => copy(headPos = headPos._1 -> (headPos._2 + 1))
      case "D" => copy(headPos = headPos._1 -> (headPos._2 - 1))
      case "L" => copy(headPos = (headPos._1 - 1) -> headPos._2)
      case "R" => copy(headPos = (headPos._1 + 1) -> headPos._2)
    }
    def shouldMoveTail: Boolean = Math.abs(tailPos._1 - headPos._1) > 1 || Math.abs(tailPos._2 - headPos._2) > 1
    def moveTail: State = {
      if (shouldMoveTail) {
        val xMove = if (headPos._1 - tailPos._1 > 0) 1 else if (headPos._1 == tailPos._1) 0 else -1
        val yMove = if (headPos._2 - tailPos._2 > 0) 1 else if (headPos._2 == tailPos._2) 0 else -1
        val newTailPos = (tailPos._1 + xMove) -> (tailPos._2 + yMove)
        copy(headPos, newTailPos, tailVisited + newTailPos)
      } else this
    }
  }

  val endState = input.foldLeft(State(0 -> 0, 0 -> 0, Set(0 -> 0)))((state, direction) => state.move(direction))
  println(endState.tailVisited.size)

  case class State2(knots: Vector[(Int, Int)], tailVisited: Set[(Int, Int)]) {
    def move(direction: String): State2 = knots.indices.foldLeft(this)((state, index) => state.moveKnot(index, direction))

    def moveHead(direction: String): State2 = direction match {
      case "U" => copy(knots.updated(0, knots(0)._1 -> (knots(0)._2 + 1)))
      case "D" => copy(knots.updated(0, knots(0)._1 -> (knots(0)._2 - 1)))
      case "L" => copy(knots.updated(0, (knots(0)._1 - 1) -> knots(0)._2))
      case "R" => copy(knots.updated(0, (knots(0)._1 + 1) -> knots(0)._2))
    }

    def shouldMoveKnot(index: Int): Boolean = Math.abs(knots(index)._1 - knots(index - 1)._1) > 1 || Math.abs(knots(index)._2 - knots(index - 1)._2) > 1

    def moveKnot(index: Int, direction: String): State2 = {
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

  val endState2 = input.foldLeft(State2(Vector.fill(10)(0 -> 0), Set(0 -> 0))){(state, direction) => val step = state.move(direction); println(step); step }
  println(endState2.tailVisited.size)
