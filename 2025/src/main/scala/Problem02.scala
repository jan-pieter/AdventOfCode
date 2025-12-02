import scala.io.Source

object Problem02 extends App:
//  val file = "02-test.txt"
  val file = "02-input.txt"
  val input = Source.fromResource(file).getLines().toVector.head.split(",").toVector.map{
    case s"$from-$to" => from.toLong -> to.toLong
  }
  println(input)

  val result = input.flatMap {
    case (f, t) => (f to t).toVector
  }.filter(isInvalid1)
//  result.foreach(println(_))
  println(result.sum)

  val result2 = input.flatMap {
    case (f, t) => (f to t).toVector
  }.filter(isInvalid2)
//    result2.foreach(println(_))
  println(result2.sum)

  def isInvalid2(l: Long): Boolean = {
    val s = l.toString
    (1 to s.length/2).exists(len => s.grouped(len).forall(_ == s.take(len)))
  }

  def isInvalid1(l: Long): Boolean = {
    val s = l.toString
    s.take(s.length/2) == s.drop(s.length / 2)
  }

  println(isInvalid2(11))
  println(isInvalid2(111))
  println(isInvalid2(211))
  println(isInvalid2(1212))
  println(isInvalid2(123123))
  println(isInvalid2(123456))
