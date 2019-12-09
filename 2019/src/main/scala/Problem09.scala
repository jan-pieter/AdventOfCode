import scala.collection.mutable
import scala.io.Source

object Problem09 extends App {

  val intputRegister = Source.fromResource("09-input.txt").getLines().next().split(",").map(_.toLong)

  val register = Array.fill(1000000)(0L)
  Array.copy(intputRegister, 0, register, 0, intputRegister.length)

  val processor = new IntCodeProcessor(register, mutable.Queue(2))

  var output = 0L
  while (output != Long.MinValue) {
    output = processor.run()
    println(s"Output $output")
  }

}
