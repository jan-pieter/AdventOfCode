import scala.collection.mutable

class IntCodeProcessor(register: Array[Long], input: mutable.Queue[Long] = mutable.Queue.empty) {
  var position = 0
  var relativeBase = 0
  var done = false

  case class Instruction(parameters: Int, execute: Int => Unit, increasePosition: Boolean = true)

  def run(): Long = {
    val allInstructions = Map(
      1 -> Instruction(3, code => register(paramIndex(code, 3)) = paramValue(code, 1) + paramValue(code, 2)),
      2 -> Instruction(3, code => register(paramIndex(code,3)) = paramValue(code, 1) * paramValue(code, 2)),
      3 -> Instruction(1, code => register(paramIndex(code, 1)) = input.dequeue()),
      4 -> Instruction(1, code => { val result = paramValue(code, 1); position += 2; return result }),
      5 -> Instruction(2, code => if (paramValue(code, 1) != 0L) position = paramValue(code, 2).toInt else position += 3, increasePosition = false),
      6 -> Instruction(2, code => if (paramValue(code, 1) == 0L) position = paramValue(code, 2).toInt else position += 3, increasePosition = false),
      7 -> Instruction(3, code => register(paramIndex(code, 3)) = if (paramValue(code, 1) < paramValue(code, 2)) 1 else 0),
      8 -> Instruction(3, code => register(paramIndex(code, 3)) = if (paramValue(code, 1) == paramValue(code, 2)) 1 else 0),
      9 -> Instruction(1, code => relativeBase += paramValue(code, 1).toInt),
      99 -> Instruction(0, _ => done = true)
    )

    while (!done) {
      val code = register(position).toInt
      val instruction = allInstructions(code % 100)
      instruction.execute(code)
      if (instruction.increasePosition)
        position += instruction.parameters + 1
    }

    Long.MinValue
  }

  private def paramValue(code: Int, parameter: Int): Long = register(paramIndex(code, parameter))

  private def paramIndex(code: Int, parameter: Int): Int = {
    (code / Math.pow(10, parameter+1).toInt) % 10 match {
      case 0 => register(position + parameter).toInt
      case 1 => position + parameter
      case 2 => relativeBase + register(position + parameter).toInt
    }
  }
}
