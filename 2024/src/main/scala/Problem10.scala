import scala.io.Source

object Problem10 extends App:
//  val file = "10-test.txt"
  val file = "10-input.txt"
  val input = Source.fromResource(file).getLines().toVector.map(_.toVector.map(_.asDigit))

  def neighbours(y: Int, x: Int): Vector[(Int, Int)] = for {
    (dy, dx) <- Vector((-1, 0), (1, 0), (0, -1), (0, 1))
    if y + dy >= 0 && y + dy < input.length
    if x + dx >= 0 && x + dx < input(y + dy).length
    if input(y + dy)(x + dx) == input(y)(x) + 1
  } yield (y + dy, x + dx)

  def topsReachable(y: Int, x: Int): Set[(Int, Int)] = {
    val height = input(y)(x)
    if height == 9 then Set((y, x)) else
      neighbours(y, x).toSet.flatMap(n => topsReachable(n._1, n._2))
  }

  val trailHeads = (for {
    y <- input.indices
    x <- input(y).indices
    if input(y)(x) == 0
  } yield (y, x)).toVector

  val result = trailHeads.map((y, x) => topsReachable(y, x).size)
  println(result.sum)

  def pathsToTop(y: Int, x: Int): Int = {
    val height = input(y)(x)
    if height == 9 then 1 else
      neighbours(y, x).map(n => pathsToTop(n._1, n._2)).sum
  }

  val result2 = trailHeads.map((y, x) => pathsToTop(y, x))
  println(result2.sum)
