import scala.io.Source

object Problem06 extends App:
//  val file = "06-test.txt"
  val file = "06-input.txt"
  val input = Source.fromResource(file).getLines().toVector.map(_.split(" ").toVector.filter(_.nonEmpty))
  //println(input)

  val results = input.transpose.map(_.reverse).map {
    case v if v.head == "*" => v.drop(1).map(_.toLong).product
    case v => v.drop(1).map(_.toLong).sum
  }

  println(results.sum)

  def fixLengths(lines: Vector[String]): Vector[String] = lines.map(_.padTo(lines.map(_.length).max, ' '))

  val input2 = fixLengths(Source.fromResource(file).getLines().toVector)
  val operators = input2.takeRight(1).map(_.split(" ").toVector.filter(_.nonEmpty)).head
  val numbers = input2.dropRight(1).transpose.foldLeft(Vector(Vector.empty[String])) {
    case (acc, next) if next.forall(_ == ' ') => acc.prepended(Vector.empty[String])
    case (acc, next) => acc.updated(0, acc.head.appended(next.mkString))
  }.reverse

  val results2 = operators.zip(numbers).map {
    case ("*", numbers) => numbers.map(_.trim.toLong).product
    case (_, numbers) => numbers.map(_.trim.toLong).sum
  }
  println(results2.sum)

