import scala.collection.mutable
import scala.io.Source

object Problem16 extends App:
//  val file = "16-test.txt"
  val file = "16-input.txt"
  val input = Source.fromResource(file).getLines().toVector
  val startY = input.indexWhere(_.contains("S"))
  val startX = input(startY).indexOf("S")
  val startDir = (0, 1)
  val endY = input.indexWhere(_.contains("E"))
  val endX = input(endY).indexOf("E")

  case class State(y: Int, x: Int, dir: (Int, Int), path: Vector[(Int, Int)], scoreSoFar: Long)
  given Ordering[State] with {
    def compare(x: State, y: State): Int = y.scoreSoFar.compareTo(x.scoreSoFar)
  }

  def left(dir: (Int, Int)): (Int, Int) = dir match {
    case (0, 1) => (-1, 0)
    case (1, 0) => (0, 1)
    case (0, -1) => (1, 0)
    case (-1, 0) => (0, -1)
  }
  def right(dir: (Int, Int)): (Int, Int) = dir match {
    case (0, 1) => (1, 0)
    case (1, 0) => (0, -1)
    case (0, -1) => (-1, 0)
    case (-1, 0) => (0, 1)
  }

  def shortestPaths: (Long, Int) = {
    var done = false
    var minCost = Long.MaxValue
    val visited = mutable.Map[((Int, Int), (Int, Int)), Long](((startX, startY), startDir) -> 0L)
    val shortestPaths = mutable.Set.empty[(Int, Int)]
    val queue = mutable.PriorityQueue(State(startY, startX, startDir, Vector((startY, startX)), 0L))
    while (!done) {
      val next = queue.dequeue()
      if (next.scoreSoFar > minCost)
        done = true
      else if next.y == endY && next.x == endX then
        minCost = next.scoreSoFar
        shortestPaths.addAll(next.path)
      else
        val forward = Option.when(
          input(next.y + next.dir._1)(next.x + next.dir._2) != '#' &&
            visited.get((next.y + next.dir._1, next.x + next.dir._2) -> next.dir).forall(_ == next.scoreSoFar + 1)
        )(
          State(next.y + next.dir._1, next.x + next.dir._2, next.dir, next.path.appended((next.y + next.dir._1, next.x + next.dir._2)), next.scoreSoFar+1)
        )
        val turnLeft = Option.when(
          visited.get((next.y , next.x) -> left(next.dir)).forall(_ == next.scoreSoFar + 1000) &&
            input(next.y + left(next.dir)._1)(next.x + left(next.dir)._2) != '#'
        )(
          State(next.y, next.x, left(next.dir), next.path, next.scoreSoFar + 1000)
        )
        val turnRight = Option.when(
          visited.get((next.y , next.x) -> right(next.dir)).forall(_ == next.scoreSoFar + 1000) &&
            input(next.y + right(next.dir)._1)(next.x + right(next.dir)._2) != '#'
        )(
          State(next.y, next.x, right(next.dir), next.path, next.scoreSoFar + 1000)
        )
        val actions = Vector(forward, turnLeft, turnRight).flatten
        visited.addOne(((next.y, next.x), next.dir) -> next.scoreSoFar)
        queue.enqueue(actions*)
    }
    (minCost, shortestPaths.size)
  }

  val start2 = System.currentTimeMillis()
  val result2 = shortestPaths
  val end2 = System.currentTimeMillis()
  println(result2)
  println(s"Time taken: ${end2 - start2}ms")
