import scala.io.Source

object Problem07 extends App:
//  val file = "07-test.txt"
  val file = "07-input.txt"
  val input = Source.fromResource(file).getLines().toVector.map(_.toVector)
//  println(input)

  val result = input.drop(1).foldLeft(Set(input.head.indexOf('S')) -> 0){
    case ((beams, times), line) =>
      beams.flatMap{index => line(index) match {
        case '.' => Set(index)
        case '^' => Set(index-1, index+1)
      }} -> (times + beams.toVector.map{index => line(index) match {
        case '.' => 0
        case '^' => 1
      }}.sum)
  }

  println(result._2)

  val result2 = input.drop(1).foldLeft(Map(input.head.indexOf('S') -> 1L)) {
    case (acc, line) =>
      acc.toVector.flatMap { (index, timelines) =>
        line(index) match {
          case '.' => Vector(index -> timelines)
          case '^' => Vector(index - 1 -> timelines, index + 1 -> timelines)
        }
      }.groupBy(_._1).map((index, values) => index -> values.map(_._2).sum)
  }

  println(result2.values.sum)

