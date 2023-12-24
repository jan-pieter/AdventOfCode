import scala.io.Source
import scala.math.BigDecimal.RoundingMode

object Problem24 extends App:
  def intersection(a: BigDecimal, c: BigDecimal, b: BigDecimal, d: BigDecimal): Option[(BigDecimal, BigDecimal)] =
    if a == b then None else
      val x = (d - c) / (a - b)
      Some(x -> (a * x + c))

  case class Hailstone(x: BigDecimal, y: BigDecimal, z: BigDecimal, vx: Int, vy: Int, vz: Int):
    def collides(hailstone: Hailstone): Option[(BigDecimal, BigDecimal, BigDecimal)] = {
      val intersections = Vector(
        intersection(BigDecimal(vx), x, BigDecimal(hailstone.vx), hailstone.x),
        intersection(BigDecimal(vy), y, BigDecimal(hailstone.vy), hailstone.y),
        intersection(BigDecimal(vz), z, BigDecimal(hailstone.vz), hailstone.z)
      )
      val intersectionTimes = intersections.flatten.map(_._1).distinct
      Option.when(intersectionTimes.size == 1 && intersectionTimes.head >= 0)((
        intersections(0).map(_._2).getOrElse(intersectionTimes.head * BigDecimal(vx) + x),
        intersections(1).map(_._2).getOrElse(intersectionTimes.head * BigDecimal(vy) + y),
        intersections(2).map(_._2).getOrElse(intersectionTimes.head * BigDecimal(vz) + z)
      ))
    }
    def intersectsPath(hailstone: Hailstone): Option[(BigDecimal, BigDecimal)] = {
      val cross = intersection(path._1, path._2, hailstone.path._1, hailstone.path._2)
      cross.filter((cx, cy) =>
        ((cx <= x && vx <= 0) || (cx >= x && vx >= 0)) &&
          ((cx <= hailstone.x && hailstone.vx <= 0) || (cx >= hailstone.x && hailstone.vx >= 0))
      )
    }
    val path: (BigDecimal, BigDecimal) = {
      val tAt0: BigDecimal = (-1 * x)/BigDecimal(vx)
      val yAt0 = tAt0 * BigDecimal(vy) + y
      BigDecimal(if tAt0 >= 0 then -vy else vy) / BigDecimal(vx.abs) -> yAt0
    }
    def atTime(t: BigDecimal): (BigDecimal, BigDecimal, BigDecimal) = (t * vx + x, t * vy + y, t * vz + z)

  object Hailstone:
    def fromString(s: String): Hailstone = s match {
      case s"$x, $y, $z @ $vx, $vy, $vz" => Hailstone(BigDecimal(x), BigDecimal(y), BigDecimal(z), vx.trim.toInt, vy.trim.toInt, vz.trim.toInt)
    }

  val input = Source.fromResource("24-input.txt").getLines().toVector.map(Hailstone.fromString)
//  input.foreach(stone => println(s"$stone ${stone.path}"))
  val min = BigDecimal("200000000000000")
  val max = BigDecimal("400000000000000")
//  val min = BigDecimal("7")
//  val max = BigDecimal("27")
  val pathIntersections = input.combinations(2).map(_.toVector).flatMap {
    case Vector(a, b) =>
      val result = a.intersectsPath(b)
//      println(s"$a $b => $result")
      result
  }
  val answer1 = pathIntersections.count((x, y) =>
    x >= min && y >= min && x <= max && y <= max
  )
  println(answer1)
  def isAnswer(stone: Hailstone): Boolean = input.forall(_.collides(stone).isDefined)
  val stone = Hailstone(BigDecimal(24), BigDecimal(13), BigDecimal(10), -3, 1, 2)
  println(isAnswer(stone))

  val equalVx = input.sortBy(_.vx).sliding(2).toVector.filter {
    case Vector(stone1, stone2) => stone1.vx == stone2.vx
  }
  val equalVy = input.sortBy(_.vy).sliding(2).toVector.filter {
    case Vector(stone1, stone2) => stone1.vy == stone2.vy
  }
  val equalVz = input.sortBy(_.vz).sliding(2).toVector.filter {
    case Vector(stone1, stone2) => stone1.vz == stone2.vz
  }

  val validVxs = equalVx.map {
    case Vector(stone1, stone2) =>
      val distanceX: BigDecimal = stone1.x-stone2.x
      (-1000 to 1000).filter(vx => vx != stone1.vx && vx != 0 && (distanceX / BigDecimal(vx - stone1.vx)).isWhole)
  }.reduce(_ intersect _)
  println(validVxs)
  val validVys = equalVy.map {
    case Vector(stone1, stone2) =>
      val distanceY: BigDecimal = stone1.y - stone2.y
      (-1000 to 1000).filter(vy => vy != stone1.vy && vy != 0 && (distanceY / BigDecimal(vy - stone1.vy)).isWhole)
  }.reduce(_ intersect _)
  println(validVys)
  val validVzs = equalVz.map {
    case Vector(stone1, stone2) =>
      val distanceZ: BigDecimal = stone1.z - stone2.z
      (-1000 to 1000).filter(vz => vz != stone1.vz && vz != 0 && (distanceZ / BigDecimal(vz - stone1.vz)).isWhole)
  }.reduce(_ intersect _)
  println(validVzs)

  // Calculate xy from first two lines
  val stone1 = input(0)
  val stone2 = input(1)

  val ma = BigDecimal(stone1.vy-validVys.head)/BigDecimal(stone1.vx-validVxs.head)
  val mb = BigDecimal(stone2.vy-validVys.head)/BigDecimal(stone2.vx-validVxs.head)
//  println(s"ma $ma mb $mb")
  val ca = stone1.y - (ma * stone1.x)
  val cb = stone2.y - (mb * stone2.x)
  val x = ((cb-ca)/(ma-mb)).setScale(0, RoundingMode.HALF_UP) // Rounding just to get rid of the .000000000
  val y = (ma*x + ca).setScale(0, RoundingMode.HALF_UP) // Rounding just to get rid of the .000000000
  val t = ((x - stone1.x)/(stone1.vx - validVxs.head)).setScale(0, RoundingMode.HALF_UP) // Rounding just to get rid of the .000000000
//  println(s"t: $t")
  val z = stone1.z + (stone1.vz - validVzs.head)*t
  val myStone = Hailstone(x, y, z, validVxs.head, validVys.head, validVzs.head)
  println(myStone)
  println(isAnswer(myStone))
  println(myStone.x + myStone.y + myStone.z)
