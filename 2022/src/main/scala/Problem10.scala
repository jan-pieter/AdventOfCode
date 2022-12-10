import scala.io.Source

object Problem10 extends App:
  sealed trait Operation:
    def cycles: Long
    def op: Long => Long

  case object Noop extends Operation:
    val cycles: Long = 1
    val op: Long => Long = identity

  case class Addx(amount: Long) extends Operation:
    val cycles = 2
    val op: Long => Long = x => x + amount

  val input = Source.fromResource("10-input.txt").getLines().toVector.map {
    case "noop" => Noop
    case s"addx $amount" => Addx(amount.toLong)
  }

  def atCycle(cycle: Long): Long =
    case class State(cycle: Long, register: Long, done: Boolean)
    input.foldLeft(State(0L, 1L, false)) {
      case (state, operation) if state.done => state
      case (state, operation) if operation.cycles + state.cycle >= cycle => state.copy(done = true)
      case (state, operation) => state.copy(cycle = state.cycle + operation.cycles, register = operation.op(state.register))
    }.register

  val solution1 = (20 to 220 by 40).map(c => c * atCycle(c)).sum
  println(solution1)

  val solution2 = (1 to 240).map { i =>
    val pixel = (i - 1) % 40
    val register = atCycle(i)
    if (pixel == register - 1 || pixel == register || pixel == register +1) '#' else '.'
  }
  solution2.grouped(40).foreach(line => println(line.mkString))
