import scala.io.Source

object Problem01 extends App {

  val input = Source.fromResource("01-input.txt").getLines().map(_.toInt).toVector

  val addedAndMultiplied = for {
    x <- input.indices
    y <- x until input.length
  } yield {
    input(x) + input(y) -> input(x) * input(y)
  }

  println(addedAndMultiplied.find(_._1 == 2020))

  val addedAndMultiplied3 = for {
    x <- input.indices
    y <- x until input.length
    z <- y until input.length
  } yield {
    input(x) + input(y) + input(z) -> input(x) * input(y) * input(z)
  }

  println(addedAndMultiplied3.find(_._1 == 2020))


}
