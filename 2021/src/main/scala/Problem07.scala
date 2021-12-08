import scala.io.Source

object Problem07 extends App {

  val input: Vector[Int] = Source.fromResource("07-input.txt").getLines().take(1).toVector.head.split(",").map(_.toInt).toVector

  val minPos: Int = input.min
  val maxPos: Int = input.max

  val all = (input.min to input.max).map { pos =>
    pos -> input.map(crab => Math.abs(crab - pos)).sum
  }

  val minCost = all.map(_._2).min

  val minCostPos = all.find(_._2 == minCost)

  println(minCostPos)


  val all2 = (input.min to input.max).map { pos =>
    pos -> input.map(crab => (0 to Math.abs(crab - pos)).sum).sum
  }

  val minCost2 = all2.map(_._2).min

  val minCostPos2 = all2.find(_._2 == minCost2)

  println(minCostPos2)




}
