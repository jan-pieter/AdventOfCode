import scala.io.Source

object Problem02 extends App {

  case class Line(min: Int, max: Int, char: Char, password: String) {
    def isValid: Boolean = {
      val count = password.count(_ == char)
      count >= min && count <= max
    }
    def isValid2: Boolean = {
      password.length >= min && password.length >= max && (password(min-1) == char || password(max-1) == char) && password(min-1) != password(max-1)
    }
  }
  object Line {
    private val pattern = "([0-9]+)-([0-9]+) ([A-Za-z]): ([A-Za-z]+)".r
    def fromString(str: String): Line = {
      val pattern(min, max, char, password) = str
      Line(min.toInt, max.toInt, char(0), password)
    }
  }

  val input = Source.fromResource("02-input.txt").getLines().map(Line.fromString).toVector

  println(input.count(_.isValid))

  println(input.count(_.isValid2))


}
