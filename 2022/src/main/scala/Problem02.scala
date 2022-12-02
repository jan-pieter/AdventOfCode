import scala.io.Source

object Problem02 extends App:
  val input: Vector[(String, String)] = Source.fromResource("02-input.txt").getLines().toVector.map {
    case s"$a $b" => a -> b
  }

  def normalize(tuple: (String, String)): (String, String) = tuple match {
    case (a, "X") => a -> "A"
    case (a, "Y") => a -> "B"
    case (a, "Z") => a -> "C"
  }

  def score1(tuple: (String, String)): Long = tuple match {
    case ("A", "A") => 1 + 3
    case ("B", "B") => 2 + 3
    case ("C", "C") => 3 + 3
    case ("A", "B") => 2 + 6
    case ("A", "C") => 3 + 0
    case ("B", "A") => 1 + 0
    case ("B", "C") => 3 + 6
    case ("C", "A") => 1 + 6
    case ("C", "B") => 2 + 0
  }

  def score2(tuple: (String, String)): Long = tuple match {
    case ("A", "X") => 0 + 3
    case ("A", "Y") => 3 + 1
    case ("A", "Z") => 6 + 2
    case ("B", "X") => 0 + 1
    case ("B", "Y") => 3 + 2
    case ("B", "Z") => 6 + 3
    case ("C", "X") => 0 + 2
    case ("C", "Y") => 3 + 3
    case ("C", "Z") => 6 + 1
  }

  val solution1 = input.map(normalize).map(score1).sum
  println(solution1)

  val solution2 = input.map(score2).sum
  println(solution2)
