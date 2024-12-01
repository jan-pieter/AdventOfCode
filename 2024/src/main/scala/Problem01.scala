import scala.io.Source

object Problem01 extends App:
//  val file = "01-test.txt"
  val file = "01-input.txt"
  val input = Source.fromResource(file).getLines().toVector.map {
    case s"$id1   $id2" => (id1.toInt, id2.toInt)
  }
  val list1 = input.map(_._1).sorted
  val list2 = input.map(_._2).sorted
  val result = list1.zip(list2).map((id1, id2) => (id1 - id2).abs).sum
  println(result)

  val result2 = list1.foldLeft(0L) { (acc, id1) =>
    val count2 = list2.count(_ == id1)
    val score = id1.toLong * count2
    acc + score
  }
  println(result2)
