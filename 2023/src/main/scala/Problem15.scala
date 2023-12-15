import scala.annotation.tailrec
import scala.io.Source

object Problem15 extends App:
  val input = Source.fromResource("15-input.txt").getLines().next.split(",").toVector

  @tailrec
  def hash(s: String, current: Int): Int = {
    if s.isEmpty then current else hash(s.tail, ((s.head.toInt + current) * 17) % 256)
  }

  println(input.map(s => hash(s, 0)).map(_.toLong).sum)

  @tailrec
  def hashmap(seq: Vector[String], boxes: Vector[Map[String, (Int, Int)]]): Vector[Map[String, (Int, Int)]] =
    if seq.isEmpty then boxes else
      val result: Vector[Map[String, (Int, Int)]] = seq.head match {
        case s"$label-" =>
          val box = hash(label, 0)
          boxes.updated(box, boxes(box).get(label) match {
            case None => boxes(box)
            case Some((i, _)) => boxes(box).removed(label).map((key, value) => (key, (if value._1 > i then value._1-1 else value._1) -> value._2))
          })
        case s"$label=$focus" =>
          val box = hash(label, 0)
          boxes.updated(box, boxes(box).get(label) match {
            case Some((i, _)) => boxes(box).updated(label, i -> focus.toInt)
            case None => boxes(box).updated(label, boxes(box).size -> focus.toInt)
          })
      }
      hashmap(seq.tail, result)

  val result = power(hashmap(input, Vector.fill(256)(Map.empty)))
  println(result)

  def power(boxes: Vector[Map[String, (Int, Int)]]): Long =
    boxes.zipWithIndex.map((map, i) =>
      map.view.values.map(value => ((i+1) * (value._1+1) * value._2).toLong).sum
    ).sum

