import scala.io.Source

object Problem01 extends App {

  val input = Source.fromResource("01-input.txt").getLines().next()

  val result = input.foldLeft((0, input.last)) {
    case ((sum, prev), cur) if prev == cur => (sum + cur.asDigit, cur)
    case ((sum, _), cur) => (sum, cur)
  }

  println(result._1)

  val result2 = input.zipWithIndex.map {
    case (cur, index) if input.charAt((index + input.length/2) % input.length) == cur => cur.asDigit
    case _ => 0
  }

  println(result2.sum)

}
