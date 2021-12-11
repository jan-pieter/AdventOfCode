import scala.collection.mutable
import scala.io.Source

object Problem11 extends App {

  val input: Array[Array[Int]] = Source.fromResource("11-input.txt").getLines().map(_.toArray.map(_.asDigit)).toArray
  val nrOfOctopuses = input.length * input.head.length

  def step(octopuses: Array[Array[Int]]): Array[Array[Int]] = {
    val toFlash: mutable.Set[(Int, Int)] = mutable.Set.empty
    // increase all by 1
    for {
      y <- octopuses.indices
      x <- octopuses.head.indices
    } yield {
      octopuses(y)(x) = octopuses(y)(x) + 1
      if (octopuses(y)(x) == 10) toFlash.add((x, y))
    }

    def flash(x: Int, y: Int): Unit = {
      octopuses(y)(x) = 0
      for {
        yDiff <- -1 to 1
        xDiff <- -1 to 1
        if !(xDiff == 0 && yDiff == 0)
      } yield {
        val newY = y + yDiff
        val newX = x + xDiff
        if (newY >= 0 && newY < octopuses.length && newX >= 0 && newX < octopuses.head.length) {
          if (octopuses(newY)(newX) > 0) {
            octopuses(newY)(newX) = octopuses(newY)(newX) + 1
            if (octopuses(newY)(newX) == 10) flash(newX, newY)
          }
        }
      }
    }

    toFlash.foreach(point => flash(point._1, point._2))

    octopuses
  }

  val endState = (1 to 290).foldLeft((input, 0L)){
    case ((octopuses, flashes), stepNr) =>
      val newOctopuses = step(octopuses)
      if (stepNr % 1000 == 0) {
        println(s"Step $stepNr")
        newOctopuses.foreach(line => println(line.mkString))
        println("")
      }
      val flashed = newOctopuses.map(_.count(_ == 0)).sum
      if (flashed == nrOfOctopuses) {
        println(s"Flashed all: step $stepNr")
      }
      (newOctopuses, flashes + flashed)
  }

  println(s"Solution 1 : ${endState._2}")
}
