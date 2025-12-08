import scala.collection.mutable
import scala.io.Source

object Problem15 extends App {
  val input: Array[Array[Int]] = Source.fromResource("15-input.txt").getLines().map(_.toArray.map(_.asDigit)).toArray

  def lowestRisk(input: Array[Array[Int]]): Int = {
    val risk = Array.fill(input.length, input.head.length)(0)

    def neighbours(x: Int, y: Int): Set[(Int, Int)] = {
      List(-1 -> 0, 0 -> -1, 1 -> 0, 0 -> 1).flatMap {
        case (yDiff, xDiff) => risk.lift(y + yDiff).flatMap(_.lift(x + xDiff)).flatMap {
          case 0 => Some((x + xDiff, y + yDiff))
          case _ => None
        }
      }.toSet
    }

    def shortestPath(x: Int, y: Int): Int = {
      List(-1 -> 0, 0 -> -1, 1 -> 0, 0 -> 1).flatMap {
        case (yDiff, xDiff) => risk.lift(y + yDiff).flatMap(_.lift(x + xDiff)).filterNot(_ == 0)
      }.min
    }

    risk(input.length - 1)(input.head.length - 1) = input(input.length - 1)(input.head.length - 1)
    val queue: mutable.PriorityQueue[(Int, Int)] = mutable.PriorityQueue.empty(using Ordering.by(t => -1 * (input(t._2)(t._1) + shortestPath(t._1, t._2))))
    queue.addAll(neighbours(input.head.length - 1, input.length - 1))
    while (queue.nonEmpty) {
      val (x, y) = queue.dequeue()
      risk(y)(x) = shortestPath(x, y) + (if (x + y == 0) 0 else input(y)(x))
      if (x + y == 0)
        queue.dequeueAll // Done!
      else
        queue.addAll(neighbours(x, y).diff(queue.toSet))
    }
    risk(0)(0)
  }

  println(s"Min risk 1: ${lowestRisk(input)}")

  val newInput = Array.ofDim[Int](input.length*5, input.head.length*5)
  for {
    y <- newInput.indices
    x <- newInput.head.indices
  } yield {
    newInput(y)(x) = (input(y % input.length)(x % input.head.length) + x / input.head.length + y / input.length - 1) % 9 + 1
  }

  println(s"Min risk 2: ${lowestRisk(newInput)}")
}
