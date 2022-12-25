import scala.io.Source

object Problem24 extends App:
  type Valley = Vector[Vector[String]]
  val input: Valley = Source.fromResource("24-input.txt").getLines().toVector.map(_.toVector.map(_.toString))

  def nextX(x: Int) = if ((x+1) == (input.head.length - 1)) 1 else x+1
  def prevX(x: Int) = if ((x-1) == 0) input.head.length-2 else x-1
  def nextY(y: Int) = if ((y+1) == (input.length - 1)) 1 else y+1
  def prevY(y: Int) = if ((y-1) == 0) input.length-2 else y-1

  def forward(valley: Valley): Valley = (for {
    x <- 1 until (valley.head.length - 1)
    y <- 1 until (valley.length - 1)
  } yield x -> y).foldLeft(valley){
    case (newValley, (x,y)) =>
      val all = "<>^v"
      newValley.updated(y, newValley(y).updated(x, all.filter {
        case '>' => valley(y)(prevX(x)).contains('>')
        case '<' => valley(y)(nextX(x)).contains('<')
        case '^' => valley(nextY(y))(x).contains('^')
        case 'v' => valley(prevY(y))(x).contains('v')
      }))
  }

  val valleys = (1 until (input.length*input.head.length)).foldLeft(Vector(input))((valleys, _) => valleys :+ forward(valleys.last))
  println("Got valleys")

  case class Position(x: Int, y: Int)

  val start = Position(input.head.indexOf("."), 0)
  val end = Position(input.last.indexOf("."), input.length - 1)
  println(s"Start $start")
  println(s"End $end")

  case class State(step: Long, myPos: Position, reachedGoal: Boolean, reachedStart: Boolean) {
    lazy val nextStates: Vector[State] = {
      val nextValley = valleys(((step + 1) % valleys.length).toInt)
      def newReachedGoal(y: Int): Boolean = reachedGoal || (y == nextValley.length - 1)
      def newReachedStart(y: Int): Boolean = reachedStart || (reachedGoal && y == 0)
      Vector(
        Option.when(myPos.y > 0 && (nextValley(myPos.y - 1)(myPos.x) == "." || nextValley(myPos.y - 1)(myPos.x) == ""))(State(step + 1, myPos.copy(y = myPos.y - 1), newReachedGoal(myPos.y-1), newReachedStart(myPos.y-1))),
        Option.when(myPos.y < (nextValley.length - 1) && (nextValley(myPos.y + 1)(myPos.x) == "." || nextValley(myPos.y + 1)(myPos.x) == ""))(State(step + 1, myPos.copy(y = myPos.y + 1), newReachedGoal(myPos.y+1), newReachedStart(myPos.y+1))),
        Option.when(nextValley(myPos.y)(myPos.x - 1) == "")(State(step + 1, myPos.copy(x = myPos.x - 1), reachedGoal, reachedStart)),
        Option.when(nextValley(myPos.y)(myPos.x + 1) == "")(State(step + 1, myPos.copy(x = myPos.x + 1), reachedGoal, reachedStart)),
        Option.when(nextValley(myPos.y)(myPos.x) == "" || nextValley(myPos.y)(myPos.x) == ".")(State(step + 1, myPos, reachedGoal, reachedStart))
      ).flatten
    }
  }

//  valleys(1).foreach(line => println(line.map(str => if (str.isEmpty) "." else if (str.length == 1) str else str.length).mkString))

  var statesToConsider = Set(State(0L, start, false, false))

  while (statesToConsider.forall(_.myPos != end) && statesToConsider.nonEmpty) {
//     println(s"${statesToConsider.head.step}: ${statesToConsider.size}")
    statesToConsider = statesToConsider.flatMap(_.nextStates)
  }
  println(statesToConsider.head.step)

  statesToConsider = Set(State(0L, start, false, false))

  while (!statesToConsider.exists(state => state.reachedGoal && state.reachedStart && state.myPos == end) && statesToConsider.nonEmpty) {
    if (statesToConsider.head.step % 100 == 0) println(s"${statesToConsider.head.step}: ${statesToConsider.size} ${statesToConsider.exists(_.reachedGoal)} ${statesToConsider.exists(_.reachedStart)}")
    statesToConsider = statesToConsider.flatMap(_.nextStates)
  }
  println(statesToConsider.head)


