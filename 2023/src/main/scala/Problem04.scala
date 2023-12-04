import scala.io.Source

object Problem04 extends App:
  case class Card(nr: Int, winning: Vector[Int], my: Vector[Int]):
    val matching: Int = my.intersect(winning).length
    val value: Long = if matching == 0 then 0L else Math.pow(2, matching - 1).toLong

  val input = Source.fromResource("04-input.txt").getLines().toVector
  val cards = input.map:
    case s"Card $nr:$winning |$my" => Card(nr.trim.toInt, winning.grouped(3).toVector.map(_.trim.toInt), my.grouped(3).toVector.map(_.trim.toInt))

  println(cards.map(_.value).sum)

  val initialCopies = Vector.fill(cards.length)(1L)
  val copies = cards.zipWithIndex.foldLeft(initialCopies):
    case (state, (card, index)) =>
      (1 to card.matching).foldLeft(state)( (updated, i) => updated.updated(index + i, updated(index+i) + updated(index)))

  println(copies.sum)
