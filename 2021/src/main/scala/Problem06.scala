import scala.io.Source

object Problem06 extends App {

  val input: Vector[Int] = Source.fromResource("06-input.txt").getLines().take(1).toVector.head.split(",").map(_.toInt).toVector

  val days = 80

  val end = (1 to days).foldLeft(input){
    case (population, _) => population.flatMap {
      case 0 => Vector(6, 8)
      case i => Vector(i-1)
    }
  }

  println(s"${end.size}")

  case class Population(day0: Long, day1: Long, day2: Long, day3: Long, day4: Long, day5: Long, day6: Long, day7: Long, day8: Long)

  val days2 = 256
  val start = Population(
    input.count(_==0),
    input.count(_==1),
    input.count(_==2),
    input.count(_==3),
    input.count(_==4),
    input.count(_==5),
    input.count(_==6),
    input.count(_==7),
    input.count(_==8)
  )

  val end2 = (1 to days2).foldLeft(start){
    case (population, _) => Population(
      population.day1,
      population.day2,
      population.day3,
      population.day4,
      population.day5,
      population.day6,
      population.day7 + population.day0,
      population.day8,
      population.day0
    )
  }

  val total = end2.day0 + end2.day1 + end2.day2 + end2.day3 + end2.day4 + end2.day5 + end2.day6 + end2.day7 + end2.day8

  println(total)
}
