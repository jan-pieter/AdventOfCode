import scala.io.Source
import scala.util.{Success, Try}

object Problem13 extends App {

  val input = Source.fromResource("13-input.txt").getLines().toVector

  val timestamp: Int = input(0).toInt
  val buses: Array[Int] = input(1).split(",").filterNot(_ == "x").map(_.toInt)
  val busesWithOffsets: Array[(Int, Int)] = input(1).split(",").zipWithIndex.filterNot(_._1 == "x").map(tuple => tuple._1.toInt -> tuple._2)

  val solution = Iterator.from(timestamp).collectFirst {
    case t if buses.exists(t % _ == 0) => t -> buses.find(t % _ == 0).head
  }

  println(s"${solution.get._1} ${solution.get._1 - timestamp} ${solution.get._2} ${(solution.get._1-timestamp)*solution.get._2}")

  val solution2 = chineseRemainder(busesWithOffsets.map(_._1.toLong).toList, busesWithOffsets.map(tuple => (tuple._1 - tuple._2).toLong).toList)

  println(s"$solution2")

  def chineseRemainder(n: List[Long], a: List[Long]): Option[Long] = {
    require(n.size == a.size)
    val prod = n.product

    def iter(n: List[Long], a: List[Long], sm: Long): Long = {
      def mulInv(a: Long, b: Long): Long = {
        def loop(a: Long, b: Long, x0: Long, x1: Long): Long = {
          if (a > 1) loop(b, a % b, x1 - (a / b) * x0, x0) else x1
        }

        if (b == 1) 1
        else {
          val x1 = loop(a, b, 0, 1)
          if (x1 < 0) x1 + b else x1
        }
      }

      if (n.nonEmpty) {
        val p = prod / n.head

        iter(n.tail, a.tail, sm + a.head * mulInv(p, n.head) * p)
      } else sm
    }

    Try {
      iter(n, a, 0) % prod
    } match {
      case Success(v) => Some(v)
      case _          => None
    }
  }
}
