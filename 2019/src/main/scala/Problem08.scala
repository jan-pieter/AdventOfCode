import scala.collection.immutable
import scala.io.Source

object Problem08 extends App {

  val data: immutable.Seq[Int] = Source.fromResource("08-input.txt").getLines().next().map(_.asDigit)

  println(data.size)
  println(data.size/(25*6))

  val minZero = data.grouped(25*6).minBy(_.count(_ == 0))

  println(minZero.count(_ == 1) * minZero.count(_ == 2))

  val merged: immutable.Seq[Int] = data.grouped(25*6).reduceLeft((result, layer) => result.zip(layer).map {
    case (2, newLayer) => newLayer
    case (before, _) => before
  })

  println("Merged:")
  merged.grouped(25).toVector.map(line => println(line.map {
    case 0 => " "
    case 1 => "■"
  }.mkString))

}
