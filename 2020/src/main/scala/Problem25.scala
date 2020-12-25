import scala.io.Source

object Problem25 extends App {

  val input = Source.fromResource("25-input.txt").getLines().toVector

  val cardPublic = input(0).toLong
  val doorPublic = input(1).toLong

  def determineLoopSize(publicKey: Long): Long = {
    var loopNumber = 0L
    var result = 1L
    val subjectNumber = 7L

    while (result != publicKey) {
      result = (result * subjectNumber) % 20201227
      loopNumber = loopNumber + 1
    }

    loopNumber
  }

  def determineKey(subject: Long, loopSize: Long): Long = {
    var times = 0L
    var result = 1L
    while (times != loopSize){
      result = (result * subject) % 20201227
      times = times + 1
    }
    result
  }

  val cardLoopSize = determineLoopSize(cardPublic)
  val doorLoopSize = determineLoopSize(doorPublic)

  println(s"$cardLoopSize $doorLoopSize")

  println(determineKey(doorPublic, cardLoopSize))
  println(determineKey(cardPublic, doorLoopSize))

}
