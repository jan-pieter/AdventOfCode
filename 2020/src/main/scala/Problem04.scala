import scala.io.Source

object Problem04 extends App {

  val input = Source.fromResource("04-input.txt").getLines().toList

  def split(list: List[String]): List[List[String]] = {
    val result = list.foldLeft(List(List.empty[String])) {
      case (state, "") => List.empty[String] :: state
      case (state, input) => (input :: state.head) :: state.tail
    }
    result.reverse
  }

  def isValid(str: String): Boolean = {
    val required = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
    required.map(_ + ":").forall(str.contains)
  }

  val eyeColors: Set[String] = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
  val color = "^#[0-9a-f]{6}$".r

  def isValid2(str: String): Boolean = {
    val values: Map[String, String] = str.split(" ").map{ keyvalue =>
      val splitted = keyvalue.split(":")
      splitted(0) -> splitted(1)
    }.toMap
    values.get("byr").exists(v => v.forall(_.isDigit) && v.toInt >= 1920 && v.toInt <= 2002) &&
    values.get("iyr").exists(v => v.forall(_.isDigit) && v.toInt >= 2010 && v.toInt <= 2020) &&
    values.get("eyr").exists(v => v.forall(_.isDigit) && v.toInt >= 2020 && v.toInt <= 2030) &&
    values.get("hgt").exists {
      case v if v.takeRight(2) == "cm" => v.take(3).forall(_.isDigit) && v.take(3).toInt >= 150 && v.take(3).toInt <= 193
      case v if v.takeRight(2) == "in" => v.take(2).forall(_.isDigit) && v.take(2).toInt >= 59 && v.take(2).toInt <= 76
      case _ => false
    } &&
    values.get("hcl").exists(v => color.findFirstIn(v).isDefined) &&
    values.get("ecl").exists(eyeColors) &&
    values.get("pid").exists(v => v.length == 9 && v.forall(_.isDigit))
  }

  val entries = split(input).map(_.mkString(" "))

  println(entries.count(isValid))
  println(entries.count(isValid2))

}
