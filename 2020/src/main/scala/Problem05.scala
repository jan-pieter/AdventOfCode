import scala.io.Source

object Problem05 extends App {

  val input = Source.fromResource("05-input.txt").getLines().toList

  val binary = input.map(_.replace("F", "0").replace("B", "1").replace("L", "0").replace("R", "1"))

  case class Seat(row: Int, column: Int) {
    def id: Int = row * 8 + column
  }

  val seats = binary.map{ str =>
    val s = Seat(Integer.parseInt(str.take(7), 2), Integer.parseInt(str.takeRight(3), 2))
    //println(s"$s with id ${s.id}")
    s
  }

  val maxId: Seat = seats.maxBy(_.id)
  println(s"Max id ${maxId.id}")

  val allSeats = (for {
    row <- 0 to 127
    column <- 0 to 7
  } yield (Seat(row, column))).toSet

  val missing = allSeats.diff(seats.toSet)
  println(missing.map(seat => s"$seat ${seat.id}").mkString("\n"))

  val mySeat = missing.filter(seat => seats.exists(_.id == seat.id+1) && seats.exists(_.id == seat.id-1))
  println("MYSEAT")
  println(mySeat.map(seat => s"$seat ${seat.id}").mkString("\n"))

}
