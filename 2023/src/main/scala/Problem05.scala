import scala.io.Source

object Problem05 extends App:
  case class MapRange(destination: Long, source: Long, length: Long)
  case class Input(seeds: Vector[Long], maps: Map[String, Vector[MapRange]], sequence: Vector[String]) {
    def endNumbers: Vector[Long] = {
      sequence.drop(1).foldLeft(seeds) { (numbers, item) =>
        numbers.map(number => maps(item).reverse.dropWhile(_.source > number).headOption match {
          case None => number
          case Some(range) =>
            val diff = number - range.source
            if diff > range.length then number else range.destination + diff
        })
      }
    }
  }

  val input = Source.fromResource("05-input.txt").getLines().toVector.foldLeft(Input(Vector.empty, Map.empty, Vector("seed"))) {
    case (soFar, "") => soFar
    case (soFar, s"seeds: $seeds") => soFar.copy(seeds = seeds.split(" ").toVector.map(_.toLong))
    case (soFar, s"$source-to-$target map:") => soFar.copy(sequence = soFar.sequence.appended(target))
    case (soFar, mapRange) =>
      val splitted = mapRange.split(" ")
      val range = MapRange(splitted(0).toLong, splitted(1).toLong, splitted(2).toLong)
      soFar.copy(
      maps = soFar.maps.updated(
        soFar.sequence.last,
        soFar.maps.getOrElse(soFar.sequence.last, Vector.empty).appended(range).sortBy(_.source)
      )
    )
  }

  println(input.endNumbers.min)


