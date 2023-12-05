import scala.io.Source

object Problem05 extends App:
  val maxValue: Long = Long.MaxValue / 10
  case class MapRange(destination: Long, source: Long, length: Long) {
    def overlap(sourceRange: MapRange): MapRange = {
      if (sourceRange.destination <= source) {
        val sourceLengthLeft = sourceRange.length - (source - sourceRange.destination)
        MapRange(
          source = source,
          destination = destination,
          length = Math.min(length, sourceLengthLeft)
        )
      } else {
        val lengthLeft = length - (sourceRange.destination - source)
        MapRange(
          source = sourceRange.destination,
          destination = destination + (sourceRange.destination - source),
          length = Math.min(lengthLeft, sourceRange.length)
        )
      }
    }
  }
  case class Input(seeds: Vector[Long], maps: Map[String, Vector[MapRange]], sequence: Vector[String]) {
    def endNumbers: Vector[Long] = {
      sequence.foldLeft(seeds) { (numbers, item) =>
        numbers.map(number => maps(item).reverse.dropWhile(_.source > number).headOption match {
          case None => number
          case Some(range) =>
            val diff = number - range.source
            if diff > range.length then number else range.destination + diff
        })
      }
    }
    def endNumbers2: Vector[MapRange] = {
      val seedRanges: Vector[MapRange] = seeds.grouped(2).map {
        case Vector(start, length) => MapRange(start, start, length)
      }.toVector
      sequence.foldLeft(seedRanges) { (ranges, item) =>
        //println(s"$item: ${ranges.mkString(",")}")
        ranges.flatMap(range =>
          maps(item)
            .dropWhile(r => r.source + r.length < range.destination)
            .reverse
            .dropWhile(r => r.source > range.destination + range.length)
            .reverse
            .map(_.overlap(range)).filter(_.length > 0)
        )
      }
    }
    def withMissingRanges: Input = copy(
      maps = maps.map( (key, ranges) =>
        key -> (
          Vector(MapRange(-1, -1, ranges.head.source+1)) ++
          ranges ++ ranges.sliding(2).flatMap {
            case Vector(r1, r2) if r2.source == r1.source + r1.length => None
            case Vector(r1, r2) => Some(MapRange(r1.source+r1.length, r1.source+r1.length, r2.source - (r1.source+r1.length)))
          } ++
          Vector(MapRange(ranges.last.source+ranges.last.length, ranges.last.source+ranges.last.length, maxValue - (ranges.last.source+ranges.last.length)))
        ).sortBy(_.source)
      )
    )
  }

  val input = Source.fromResource("05-input.txt").getLines().toVector.foldLeft(Input(Vector.empty, Map.empty, Vector.empty)) {
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
//  println(input.withMissingRanges)
  println(input.withMissingRanges.endNumbers2.minBy(_.destination).destination)


