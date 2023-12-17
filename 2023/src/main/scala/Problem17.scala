import Problem17.Direction.*

import scala.collection.mutable
import scala.io.Source

object Problem17 extends App:
  val input = Source.fromResource("17-input.txt").getLines().map(_.map(_.asDigit)).toVector
  enum Direction:
    case North, East, South, West

  case class Step(x: Int, y: Int, direction: Direction, consecutive: Int, heatLoss: Long) {
    val isValid: Boolean = x >= 0 && y >= 0 && y < input.length && x < input.head.length
    def forward: Step = direction match {
      case North => copy(y = y - 1, consecutive = consecutive + 1)
      case South => copy(y = y + 1, consecutive = consecutive + 1)
      case East => copy(x = x + 1, consecutive = consecutive + 1)
      case West => copy(x = x - 1, consecutive = consecutive + 1)
    }
    def left: Step = direction match {
      case North => copy(x = x - 1, direction = West, consecutive = 1)
      case South => copy(x = x + 1, direction = East, consecutive = 1)
      case East => copy(y = y - 1, direction = North, consecutive = 1)
      case West => copy(y = y + 1, direction = South, consecutive = 1)
    }
    def right: Step = direction match {
      case North => copy(x = x + 1, direction = East, consecutive = 1)
      case South => copy(x = x - 1, direction = West, consecutive = 1)
      case East => copy(y = y + 1, direction = South, consecutive = 1)
      case West => copy(y = y - 1, direction = North, consecutive = 1)
    }
  }
  given Ordering[Step] = Ordering.by[Step, Long](_.heatLoss).reverse

  case class CacheItem(x: Int, y: Int, going: Direction, consecutive: Int)
  object CacheItem:
    def fromStep(step: Step): CacheItem = CacheItem(step.x, step.y, step.direction, step.consecutive)

  def minHeatLoss(minConsecutive: Int, maxConsecutive: Int): Long = {
    val queue = mutable.PriorityQueue(Step(0, 0, East, 0, 0), Step(0, 0, South, 0, 0))
    val cache = mutable.Set.empty[CacheItem]
    cache.addAll(queue.map(CacheItem.fromStep))

    var done = false
    var result = -1L
    while (!done) {
      val current = queue.dequeue()
//      println(current)
      if current.y == input.indices.last && current.x == input.head.indices.last && current.consecutive >= minConsecutive then
        done = true
        result = current.heatLoss
      else
        val nextStepsForward =
          if current.consecutive < maxConsecutive then Vector(current.forward) else Vector.empty
        val nextStepsTurns =
          if current.consecutive >= minConsecutive then Vector(current.left, current.right) else Vector.empty
        val nextSteps = (nextStepsForward ++ nextStepsTurns)
            .filter(_.isValid)
            .filter(step => !cache(CacheItem.fromStep(step)))
            .map(step => step.copy(heatLoss = step.heatLoss + input(step.y)(step.x)))

        cache.addAll(nextSteps.map(CacheItem.fromStep))
        queue.addAll(nextSteps)
    }
    result
  }

  println(minHeatLoss(1, 3))
  println(minHeatLoss(4, 10))
