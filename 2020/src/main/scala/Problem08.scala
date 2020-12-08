import scala.collection.mutable
import scala.io.Source

object Problem08 extends App {

  case class Instruction(operation: String, parameter: Int)
  object Instruction {
    def apply(str: String): Instruction = {
      val parts = str.split(" ")
      Instruction(parts(0), parts(1).replace("+", "").toInt)
    }
  }

  val input = Source.fromResource("08-input.txt").getLines().toVector.map(Instruction(_))

  def run(program: Vector[Instruction]): (Boolean, Long) = {
    var accumulator: Long = 0L
    var instructionPointer: Int = 0
    var done = false
    var successful = false
    val seen = mutable.Set.empty[Int]
    while (!done) {
      if (instructionPointer == program.length) {
        done = true
        successful = true
      } else if (seen.contains(instructionPointer)) {
        done = true
      } else {
        val instruction = program(instructionPointer)
        seen.add(instructionPointer)
        instruction.operation match {
          case "nop" =>
            instructionPointer = instructionPointer + 1
          case "acc" =>
            accumulator = accumulator + instruction.parameter
            instructionPointer = instructionPointer + 1
          case "jmp" =>
            instructionPointer = instructionPointer + instruction.parameter
        }
      }
    }
    (successful, accumulator)
  }

  println(run(input))

  val updatedPrograms = input.zipWithIndex.flatMap {
    case (Instruction("nop", parameter), index) => Some(input.updated(index, Instruction("jmp", parameter)))
    case (Instruction("jmp", parameter), index) => Some(input.updated(index, Instruction("nop", parameter)))
    case _ => None
  }

  updatedPrograms.find { program =>
    val result = run(program)
    if(result._1) {
      println(result._2)
    }
    result._1
  }

}
