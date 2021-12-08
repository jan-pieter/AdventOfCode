import scala.io.Source

object Problem08 extends App {

  case class Instance(patterns: Vector[String], output: Vector[String])
  val input: Vector[Instance] = Source.fromResource("08-input.txt").getLines().map { line =>
    val parts = line.split(" ")
    Instance(parts.take(10).toVector, parts.slice(11, 15).toVector)
  }.toVector

//  input.map(println(_))

  val solution1 = input.map(_.output.map(_.length).count {
    case 2 => true
    case 3 => true
    case 4 => true
    case 7 => true
    case _ => false
  }).sum

  println(s"Solution1: $solution1")

  val solution2 = input.map { instance =>
    val patterns = instance.patterns.map {
      case s if s.length == 2 => s.toSet -> "1"
      case s if s.length == 3 => s.toSet -> "7"
      case s if s.length == 4 => s.toSet -> "4"
      case s if s.length == 7 => s.toSet -> "8"
      case s if s.length == 6 => if (!instance.patterns.find(_.length == 2).get.forall(s.toSet.contains)) {
        s.toSet -> "6"
      } else if (instance.patterns.find(_.length == 4).get.forall(s.toSet.contains)) {
        s.toSet -> "9"
      } else {
        s.toSet -> "0"
      }
      case s if s.length == 5 => if (instance.patterns.find(_.length == 2).get.forall(s.toSet.contains)) {
        s.toSet -> "3"
      } else if (instance.patterns.find(_.length == 4).get.count(s.toSet.contains) == 3) {
        s.toSet -> "5"
      } else {
        s.toSet -> "2"
      }
    }.toMap

    val result = instance.output.map(digit => patterns(digit.toSet)).mkString.toInt
//    println(s"Result: $result")
    result
  }.sum

  println(s"Solution2: $solution2")


}
