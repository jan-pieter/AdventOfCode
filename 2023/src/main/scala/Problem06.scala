object Problem06 extends App:
  case class Race(time: Int, distance: Long)
  val input = Vector(Race(7, 9), Race(15, 40), Race(30, 200))
//  val input = Vector(Race(53, 275), Race(71, 1181), Race(78, 1215), Race(80, 1524))
  def simulate(charge: Long, total: Long): Long = charge * (total-charge)
  val result = input.map{ race =>
    (0 to race.time).count(simulate(_, race.time) > race.distance)
  }.product
  println(result)

  val input2 = Race(71530, 940200)
//  val input2 = Race(53717880, 275118112151524L)
  println((0 to input2.time).count(simulate(_, input2.time) > input2.distance))
