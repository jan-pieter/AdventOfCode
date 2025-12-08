import scala.collection.mutable
import scala.io.Source

object Problem21 extends App {

  val intputRegister = Source.fromResource("21-input.txt").getLines().next().split(",").map(_.toLong)

  val register = Array.fill(1000000)(0L)
  Array.copy(intputRegister, 0, register, 0, intputRegister.length)

//  val instructions = "NOT A J\nNOT B T\nOR T J\nNOT C T\nOR T J\nAND D J\nWALK\n"
  val instructions = "NOT A J\nNOT B T\nOR T J\nNOT C T\nOR T J\nAND D J\nNOT E T\nAND H T\nOR E T\nAND T J\nRUN\n"
//  println(instructions.map(_.toLong).mkString(", "))

  val input = mutable.Queue[Long](instructions.map(_.toLong)*)
  val processor = new IntCodeProcessor(register, input)

  var output = -1L
  while (output != Long.MinValue) {
    output = processor.run()
    output match {
      case Long.MinValue => //no-op
      case l if l < 128 => print(l.toChar)
      case damage => println(s"Damage: $damage")
    }
  }
}