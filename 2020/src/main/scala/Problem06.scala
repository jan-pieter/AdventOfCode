import scala.io.Source

object Problem06 extends App {

  val input = Source.fromResource("06-input.txt").getLines().toList

  def split(list: List[String]): List[List[String]] = {
    val result = list.foldLeft(List(List.empty[String])) {
      case (state, "") => List.empty[String] :: state
      case (state, input) => (input :: state.head) :: state.tail
    }
    result.reverse
  }

  val entries = split(input).map(_.mkString("").toList.distinct)

  println(entries.map(_.size).sum)

  val entries2 = split(input).map(group => group.map(_.toList).foldLeft(('a' to 'z').toSet) {
    case (found, elem) => found.intersect(elem.toSet)
  })

  println(entries2.map(_.size).sum)

}
