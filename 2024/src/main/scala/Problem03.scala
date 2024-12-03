import scala.io.Source

object Problem03 extends App:
//  val file = "03-test.txt"
  val file = "03-input.txt"
  val input = Source.fromResource(file).getLines().toVector
  val regex = "mul\\(([0-9]+),([0-9]+)\\)".r
  val matches = regex.findAllMatchIn(input.mkString("")).toVector
  println(matches.map(m => m.group(1).toLong * m.group(2).toLong).sum)

  val regex2 = "(do\\(\\)|don't\\(\\)|mul\\(([0-9]+),([0-9]+)\\))".r
  val matches2 = regex2.findAllMatchIn(input.mkString("")).toVector
  val result2 = matches2.foldLeft((0L, true)) {
    case ((r, active), m) if m.group(0) == "do()" => (r, true)
    case ((r, active), m) if m.group(0) == "don't()" => (r, false)
    case ((r, false), m) => (r, false)
    case ((r, true), m) => (r + m.group(2).toLong * m.group(3).toLong, true)
  }
  println(result2._1)
