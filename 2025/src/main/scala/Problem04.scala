import scala.io.Source

object Problem04 extends App:
//  val file = "04-test.txt"
  val file = "04-input.txt"
  val input = Source.fromResource(file).getLines().toVector.map(_.toVector)
  //println(input)

  val reachable = for {
    y <- input.indices
    x <- input(y).indices
  } yield {
    input(y)(x) == '@' && (for {
      ydiff <- -1 to 1
      xdiff <- -1 to 1
      if ydiff != 0 || xdiff != 0
    } yield {
      val newy = y + ydiff
      val newx = x + xdiff
      newy >= 0 && newy < input.length && newx >= 0 && newx < input(y).length && input(newy)(newx) == '@'
    }).count(identity) < 4
  }

  println(reachable.count(identity))

  def removeReachable(state: Vector[Vector[Char]]): Vector[Vector[Char]] = {
    val indices = for {
      y <- state.indices
      x <- state(y).indices
    } yield y -> x
    indices.foldLeft(state){
      case (acc, (y, x)) =>
       if (state(y)(x) == '@' && (for {
        ydiff <- -1 to 1
        xdiff <- -1 to 1
        if ydiff != 0 || xdiff != 0
      } yield {
        val newy = y + ydiff
        val newx = x + xdiff
        newy >= 0 && newy < state.length && newx >= 0 && newx < state(y).length && state(newy)(newx) == '@'
      }).count(identity) < 4) then acc.updated(y, acc(y).updated(x, '.')) else acc
    }
  }

  var oldInput = input
  var newInput = removeReachable(input)
  while (newInput != oldInput) {
    oldInput = newInput
    newInput = removeReachable(newInput)
  }
  val removed = input.map(_.count(_ == '@')).sum - newInput.map(_.count(_ == '@')).sum
  println(removed)
