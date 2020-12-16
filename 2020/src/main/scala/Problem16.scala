import scala.collection.mutable
import scala.io.Source

object Problem16 extends App {

  val input = Source.fromResource("16-input.txt").getLines().toList

  def split(list: List[String]): List[List[String]] = {
    val result = list.foldLeft(List(List.empty[String])) {
      case (state, "") => List.empty[String] :: state
      case (state, input) => (input :: state.head) :: state.tail
    }
    result.reverse.map(_.reverse)
  }

  case class Field(name: String, bounds: List[(Int, Int)]) {
    def isValidValue(value: Int): Boolean = {
      bounds.exists(bound => bound._1 <= value && bound._2 >= value)
    }
  }

  case class Ticket(values: List[Int])

  val splittedInput = split(input)

  val fields: List[Field] = splittedInput.head.map { str =>
    val strFields = str.split(": ")
    val boundStrs = strFields(1).split(" or ")
    val bounds = boundStrs.map { boundStr =>
      val values = boundStr.split("-")
      values(0).toInt -> values(1).toInt
    }.toList

    Field(strFields(0), bounds)
  }
//  println(fields)

//  println(splittedInput(1))

  val myTicket: Ticket = Ticket(splittedInput(1)(1).split(",").map(_.toInt).toList)

  val otherTickets: List[Ticket] = splittedInput(2).drop(1).map(str => Ticket(str.split(",").map(_.toInt).toList))

  val invalidValues: List[Int] = otherTickets.flatMap(_.values).filter(value => fields.forall(!_.isValidValue(value)))

  println(invalidValues.sum)

  val validTickets: List[Ticket] = otherTickets.filterNot(_.values.exists(value => fields.forall(!_.isValidValue(value))))

  val allTickets: List[Ticket] = myTicket :: validTickets

  val positions: Map[Field, List[Int]] = fields.map{ field =>
    field -> myTicket.values.indices.filter { index =>
      val values = allTickets.map(_.values(index))
      values.forall(field.isValidValue)
    }.toList
  }.toMap

//  println(positions)

  val fieldPositions: mutable.Map[Field, Int] = mutable.Map.empty
  val assigned: mutable.Set[Int] = mutable.Set.empty

  while (fieldPositions.size != fields.length) {

    val toConsider = positions.filterKeys(!fieldPositions.contains(_)).mapValues(_.filterNot(assigned.contains))
    val (field, index) = toConsider.collectFirst {
      case (field, indices) if indices.size == 1 => field -> indices.head
    }.orElse{
      fields.indices.filterNot(assigned).collectFirst {
        case index if toConsider.exists(_._2 == List(index)) =>
          val (field, indices) = toConsider.find(_._2 == List(index)).get
          field -> indices.head
      }
    }.getOrElse(throw new IllegalArgumentException("No candidate found"))
    fieldPositions += field -> index
    assigned += index
  }

  println(fieldPositions)

  val departureValues = fieldPositions.filterKeys(_.name.startsWith("departure")).map {
    case (_, index) => myTicket.values(index)
  }

  println(departureValues)

  println(departureValues.map(_.toLong).product)


}
