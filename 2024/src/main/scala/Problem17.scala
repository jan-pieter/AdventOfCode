import scala.io.Source

object Problem17 extends App:
//  val file = "17-test.txt"
  val file = "17-input.txt"
  val input = Source.fromResource(file).getLines().toVector

  case class Computer(a: Long, b: Long, c: Long, instruction: Int, program: Vector[(Int, Int)], output: Vector[Long]) {
    def combo(op: Int): Long = op match {
      case 4 => a
      case 5 => b
      case 6 => c
      case other => other
    }
    def next: Computer = {
      val (ins, op) = program(instruction)
      ins match {
        case 0 => copy(a = a / Math.pow(2L, combo(op).toDouble).toLong, instruction = instruction + 1)
        case 1 => copy(b = b ^ op, instruction = instruction + 1)
        case 2 => copy(b = combo(op) % 8L, instruction = instruction + 1)
        case 3 => if (a == 0L) copy(instruction = instruction + 1) else copy(instruction = op)
        case 4 => copy(b = b ^ c, instruction = instruction + 1)
        case 5 => copy(output = output.appended(combo(op) % 8L), instruction = instruction + 1)
        case 6 => copy(b = a / Math.pow(2L, combo(op).toDouble).toLong, instruction = instruction + 1)
        case 7 => copy(c = a / Math.pow(2L, combo(op).toDouble).toLong, instruction = instruction + 1)
      }
    }
  }

  val a: Long = input.find(_.startsWith("Register A")).get.drop("Register A: ".length).toLong
  val b: Long = input.find(_.startsWith("Register B")).get.drop("Register B: ".length).toLong
  val c: Long = input.find(_.startsWith("Register C")).get.drop("Register C: ".length).toLong
  val program: Vector[(Int, Int)] = input.find(_.startsWith("Program")).get.drop("Program: ".length).split(",").grouped(2).map {
    case Array(ins, op) => (ins.toInt, op.toInt)
  }.toVector
  println(s"Registers: A: $a, B: $b, C: $c")
  println(s"Program: $program")
  val computer = Computer(a, b, c, 0, program, Vector.empty)
  def run(computer: Computer): Computer = {
    LazyList.iterate(computer)(_.next).find(_.instruction >= program.length).get
  }
  val result = run(computer)
  println(result.output.mkString(","))

  // Some analysis
  val programV = program.flatMap { case (ins, op) => Vector(ins, op) }
  var initA = 0L
  while (initA < 100L) {
    val computer = Computer(initA, b, c, 0, program, Vector.empty)
    val result = run(computer)

    if result.output.length > 2 && result.output.takeRight(3) == programV.takeRight(3) then
      println(s"Last3: $initA => ${initA.toBinaryString}")
    else if result.output.length > 1 && result.output.takeRight(2) == programV.takeRight(2) then
      println(s"Last2: $initA => ${initA.toBinaryString}")
    else if result.output.length > 0 && result.output.takeRight(1) == programV.takeRight(1) then
      println(s"Last1: $initA => ${initA.toBinaryString}")

    initA += 1L
  }

  def determineOctal(currentIndex: Int, current: String): Vector[Long] = {
    if currentIndex < 0 then
      return Vector(BigInt(current, 2).toLong)
    val options = for {
      x <- List("0", "1")
      y <- List("0", "1")
      z <- List("0", "1")
    } yield s"$current$x$y$z"
    options.filter { bitstring =>
      val a = BigInt(bitstring, 2).toLong
      val result = run(Computer(a, b, c, 0, program, Vector.empty)).output
      result.length >= programV.length-currentIndex && result.takeRight(programV.length-currentIndex) == programV.drop(currentIndex)
    }.toVector.flatMap(bitstring => determineOctal(currentIndex-1, bitstring))
  }

  val result2 = determineOctal(programV.length-1, "")
  println(result2.minOption)
