import scala.io.Source

object Problem02 extends App:
  case class Game(id: Int, sets: Vector[Map[String, Int]]) {
    def isPossible(red: Int, green: Int, blue: Int): Boolean =
      sets.forall(round => round.getOrElse("red", 0) <= red && round.getOrElse("green", 0) <= green && round.getOrElse("blue", 0) <= blue)
    def required(color: String): Long = sets.map(_.getOrElse(color, 0)).max
    val power: Long = required("red") * required("green") * required("blue")
  }

  val input = Source.fromResource("02-input.txt").getLines().toVector.map {
    case s"Game $id: $sets" => Game(id.toInt, sets.split("; ").toVector.map( _.split(", ").map {
      case s"$amount $color" => color -> amount.toInt
    }.toMap))
  }

  val answer1 = input.filter(_.isPossible(12, 13, 14)).map(_.id).sum
  println(answer1)

  val answer2 = input.map(_.power).sum
  println(answer2)
