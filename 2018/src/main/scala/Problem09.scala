import scala.io.Source

object Problem09 extends App {

  val (players, lastMarble) = Source.fromResource("09-test.txt").getLines().map { line =>
    val words = line.split(" ")
    (words(0).toInt, words(6).toInt)
  }.next()

  println(s"$players players $lastMarble lastMarble")

  var circle = List(0)
  var currentMarbleIndex = 0
  val scores = Array.fill(players)(0L)

  def insert(value: Int): Unit = {
    val index = if (value == 1) 1 else (currentMarbleIndex + 2) % circle.size
    val (front, back) = circle.splitAt(index)
    println(s"Index $index Front $front Back $back")
    circle = front ++ List(value) ++ back
    currentMarbleIndex = index
  }

  for {
    marble <- 1 to lastMarble
  } yield {
    insert(marble)
    println(circle)
  }


}
