import scala.io.Source

object Problem03 extends App {

  val map = Source.fromResource("03-input.txt").getLines().map(_.toCharArray).toArray

  def treesWithSlope(stepX: Int, stepY: Int): Int = {

    var position = (0, 0) // y, x
    var trees = 0

    while (position._1 < map.length - 1) {
      position = (position._1 + stepY, position._2 + stepX)
      if (map(position._1)(position._2 % map(0).length) == '#')
        trees = trees + 1
    }

    trees
  }

  println(treesWithSlope(3, 1))

  val slope1 = treesWithSlope(1, 1).toLong
  val slope2 = treesWithSlope(3, 1).toLong
  val slope3 = treesWithSlope(5, 1).toLong
  val slope4 = treesWithSlope(7, 1).toLong
  val slope5 = treesWithSlope(1, 2).toLong
  val multiplied = slope1 * slope2 * slope3 * slope4 * slope5

  println(s"$slope1 $slope2 $slope3 $slope4 $slope5 $multiplied")

}
