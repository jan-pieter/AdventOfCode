import scala.io.Source

object Problem09 extends App {

  val input: Vector[Vector[Int]] = Source.fromResource("09-input.txt").getLines().map(_.map(_.asDigit).toVector).toVector

  val lowPoints = (for {
    y <- input.indices
    x <- input.head.indices
  } yield {
    val surrounding = (input.lift(y - 1).map(_(x)) :: input(y).lift(x - 1) :: input(y).lift(x + 1) :: input.lift(y + 1).map(_(x)) :: Nil).flatten
    Option.when(input(y)(x) < surrounding.min)(x -> y)
  }).flatten

  val solution1 = lowPoints.map {
    case (x, y) => input(y)(x) + 1
  }.sum

  println(s"Solution 1 : $solution1")

  //lowPoints.foreach(println(_))

  val bassins = lowPoints.map {
    case (x, y) => traverse(x, y, Set.empty)
  }

  //bassins.foreach(println(_))

  val solution2 = bassins.sortBy(_._1).takeRight(3).map(_._1).product

  println(s"Solution 2 : $solution2")

  def traverse(x: Int, y: Int, seen: Set[(Int, Int)]): (Int, Set[(Int, Int)]) = {
    val surrounding = x -> (y - 1) :: (x - 1) -> y :: (x + 1) -> y :: x -> (y + 1) :: Nil
    surrounding.foldLeft((1, seen + (x -> y))) {
      case ((soFar, seen), (newX, newY)) =>
        val value = input.lift(newY).flatMap(_.lift(newX)).getOrElse(9)
        if (value > input(y)(x) && value < 9 && !seen.contains(newX -> newY)) {
          val point = traverse(newX, newY, seen)
          (soFar + point._1, point._2)
        } else {
          (soFar, seen)
        }
    }
  }

}
