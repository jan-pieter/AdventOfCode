import scala.collection.mutable
import scala.io.Source

object Problem19 extends App {

  case class Position(x: Int, y: Int, z: Int) {
    def rotateZ(deg: Int): Position = deg match {
      case 0 => this
      case 90 => Position(y, -x, z)
      case 180 => Position(-x, -y, z)
      case 270 => Position(-y, x, z)
    }

    def rotateX(deg: Int): Position = deg match {
      case 0 => this
      case 90 => Position(x, z, -y)
      case 180 => Position(x, -y, -z)
      case 270 => Position(x, -z, y)
    }

    def rotateY(deg: Int): Position = deg match {
      case 0 => this
      case 90 => Position(-z, y, x)
      case 180 => Position(-x, y, -z)
      case 270 => Position(z, y, -x)
    }
  }

  case class Scanner(number: Int, beacons: Set[Position]) {
    val relatives: Set[Set[Position]] = beacons.map(beacon =>
      (beacons - beacon).map(other => Position(beacon.x - other.x, beacon.y - other.y, beacon.z - other.z))
    )
    val relatives2: Map[Position,Set[Position]] = beacons.map(beacon =>
      beacon -> (beacons - beacon).map(other => Position(beacon.x - other.x, beacon.y - other.y, beacon.z - other.z))
    ).toMap

    def rotated(xDeg: Int, yDeg: Int, zDeg: Int): Scanner = copy(beacons = beacons.map(_.rotateX(xDeg).rotateY(yDeg).rotateZ(zDeg)))
    def shifted(xDiff: Int, yDiff: Int, zDiff: Int): Scanner = copy(beacons = beacons.map(p => Position(p.x + xDiff, p.y + yDiff, p.z + zDiff)))

    def overlap(other: Scanner): Option[Scanner] = {
      (for {
        xDeg <- List(0, 90, 180, 270)
        yDeg <- List(0, 90, 180, 270)
        zDeg <- List(0, 90, 180, 270)
      } yield {
        val otherRotated = other.rotated(xDeg, yDeg, zDeg)
        Option.when(relatives.count(beaconRelatives =>
          otherRotated.relatives.exists(otherRelatives =>
            beaconRelatives.intersect(otherRelatives).size >= 11
          )
        ) >= 12)(otherRotated)
      }).flatten.headOption
    }

    def overlap2(other: Scanner): Option[(Int, Int, Int)] = {
      (for {
        xDeg <- List(0, 90, 180, 270)
        yDeg <- List(0, 90, 180, 270)
        zDeg <- List(0, 90, 180, 270)
      } yield {
        Option.when(relatives.count(beaconRelatives =>
          other.rotated(xDeg, yDeg, zDeg).relatives.exists(otherRelatives =>
            beaconRelatives.intersect(otherRelatives).size >= 11
          )
        ) >= 12)((xDeg, yDeg, zDeg))
      }).flatten.headOption
    }
  }

  val input = Source.fromResource("19-test.txt").getLines().toVector
  val scanners = input.filter(_.nonEmpty).foldLeft(Vector.empty[Scanner]) {
    case (scanners, s"--- scanner $i ---") => scanners :+ Scanner(i.toInt, Set.empty)
    case (scanners, s"$x,$y,$z") => scanners.updated(scanners.length - 1, scanners.last.copy(beacons = scanners.last.beacons + Position(x.toInt, y.toInt, z.toInt)))
  }

  //scanners.foreach(println(_))

  println(scanners(0).overlap(scanners(1)))

  /*
  val toMatch: mutable.Set[Scanner] = mutable.Set.from(scanners.drop(1))
  val matched: mutable.Set[Scanner] = mutable.Set(scanners.head)
  while(toMatch.nonEmpty) {
    val (originalScanner, rotatedScanner) = matched.flatMap(s => toMatch.flatMap(t => s.overlap(t).map(t -> _)).headOption).head
    matched.add(rotatedScanner)
    toMatch.remove(originalScanner)
  }
  println(matched.flatMap(_.beacons).size)
  */
}
