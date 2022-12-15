import scala.io.Source

object Problem15 extends App:
  case class Sensor(x: Int, y: Int, beaconX: Int, beaconY: Int) {
    val distance: Int =  Math.abs(x - beaconX) + Math.abs(y - beaconY)
    def targetBounds(target: Int): Option[(Int, Int)] = {
      val b = Option.when(y <= target && y + distance >= target) {
        val diff = distance - (target - y)
        (x - diff, x + diff)
      }.orElse(Option.when(y > target && y - distance <= target) {
        val diff = distance - (y - target)
        (x - diff, x + diff)
      })
//      println(s"$this : $b")
      b
    }
  }
  val input = Source.fromResource("15-input.txt").getLines().toVector.map {
    case s"Sensor at x=$x, y=$y: closest beacon is at x=$beaconX, y=$beaconY" => Sensor(x.toInt, y.toInt, beaconX.toInt, beaconY.toInt)
  }

//  val target = 20
  val target = 2000000
  val beacons = input.map(sensor => sensor.beaconX -> sensor.beaconY).toSet

  val bounds = input.flatMap(_.targetBounds(target)).sortBy(_._1)
//  bounds.foreach(println(_))
  val maxBoundX = bounds.map(_._2).max
  val minBoundX = bounds.map(_._1).min
  val line = Array.fill(maxBoundX - minBoundX + 1)('.')
  bounds.foreach((x1, x2) => (x1 - minBoundX to x2 - minBoundX).foreach(x => line(x) = '#'))
  beacons.filter(_._2 == target).filter(_._1 <= maxBoundX).foreach((x, y) => line(x - minBoundX) = 'B')
//  println(line.mkString)
  val solution1 = line.count(_ == '#')
  println(solution1)

//  val target2 = 20
  val target2 = 4000000
  val empty = (0 to target2).foreach{ y =>
//    if (y % 100 == 0) print(".")
    val bounds = input.flatMap(_.targetBounds(y)).sortBy(_._1)
//    bounds.foreach(println(_))
    val beaconBounds = beacons.filter(_._2 == y).map(b => b._1 -> b._1)
    (bounds ++ beaconBounds).sortBy(_._1).foldLeft[(Int, Int)]((Int.MinValue, -1)) {
      case ((xMin, xMax), (yMin, yMax)) =>
        if (yMin > (xMax+1)) {println(s"${(yMin.toLong-1) * 4000000 + y}")}
        (xMin, Math.max(xMax, yMax))
    }
  }
