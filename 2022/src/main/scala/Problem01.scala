import scala.io.Source

object Problem01 extends App:
  val input: Vector[Vector[Int]] = Source.fromResource("01-input.txt").getLines().toVector.foldRight(Vector(Vector.empty[Int])){
    case ("", state) => Vector.empty[Int] +: state
    case (elem, state) => (elem.toInt +: state.head) +: state.tail
  }
  val solution1 = input.map(_.sum).max
  println(solution1)

  val solution2 = input.map(_.sum).sorted.takeRight(3).sum
  println(solution2)
