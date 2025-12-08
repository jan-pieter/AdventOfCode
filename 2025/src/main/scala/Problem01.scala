import scala.annotation.nowarn
import scala.io.Source

object Problem01 extends App:
//  val file = "01-test.txt"
  val file = "01-input.txt"
  val regex = "([LR])([0-9]+)".r
  val input = Source.fromResource(file).getLines().toVector.map {
    case regex(direction, amount) => (direction, amount.toInt)
  }

  @nowarn("msg=not.*?exhaustive")
  val result1 = input.foldLeft(Vector(50))((acc, next) => next match {
    case ("L", amount) => acc.prepended((acc.head - (amount % 100) + 100) % 100)
    case ("R", amount) => acc.prepended((acc.head + (amount % 100)) % 100)
  })

  println(result1.count(_ == 0))

  @nowarn("msg=not.*?exhaustive")
  val result2 = input.foldLeft(Vector((50, 0L)))((acc, next) => next match {
    case ("L", amount) =>
      val last = acc.head
      val result = (last._1 - (amount % 100) + 100) % 100 -> (last._2 + (amount / 100L) + (if last._1 != 0 && last._1 - (amount % 100) <= 0 then 1L else 0L))
//      println(s"L$amount $result")
      acc.prepended(result)
    case ("R", amount) =>
      val last = acc.head
      val result = (last._1 + (amount % 100)) % 100 -> (last._2 + (amount / 100L) + (if last._1 != 0 && last._1 + (amount % 100) >= 100 then 1L else 0L))
//      println(s"R$amount $result")
      acc.prepended(result)
  })

  println(result2.head._2)

