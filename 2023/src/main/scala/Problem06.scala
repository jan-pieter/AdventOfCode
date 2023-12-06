object Problem06 extends App:
  case class Race(time: Int, distance: Long)
  val input = Vector(Race(7, 9), Race(15, 40), Race(30, 200))
  def simulate(charge: Long, total: Long): Long = charge * (total-charge)
  val result = input.map{ race =>
    (0 to race.time).count(simulate(_, race.time) > race.distance)
  }.product
  println(result)

  val input2 = Race(71530, 940200)
  println((0 to input2.time).count(simulate(_, input2.time) > input2.distance))
