import scala.io.Source

object Problem09 extends App {

  val preamble: Int = 25
  val input = Source.fromResource("09-input.txt").getLines().toVector.map(_.toLong)

  val invalid: Long = (for {
    i <- preamble until input.length
  } yield {
    val valid = (for {
      x <- (i - preamble) until (i - 1)
      y <- (x + 1) until i
    } yield {
      input(x) + input(y)
    }).contains(input(i))
    if (!valid) Some(input(i))
    else None
  }).flatten.head

  println(invalid)

  for {
    i <- 0 until (input.length - 1)
  } yield {
    var sum = input(i)
    var smallest = input(i)
    var largest = input(i)
    for {
      j <- (i+1) until input.length
    } yield {
      sum = sum + input(j)
      smallest = Math.min(smallest, input(j))
      largest = Math.max(largest, input(j))
      if (sum == invalid) {
        println(s"$smallest $largest ${smallest+largest}")
      }
    }
  }

}
