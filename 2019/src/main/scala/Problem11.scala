import scala.collection.mutable
import scala.io.Source

object Problem11 extends App {

  val intputRegister = Source.fromResource("11-input.txt").getLines().next().split(",").map(_.toLong)

  val register = Array.fill(1000000)(0L)
  Array.copy(intputRegister, 0, register, 0, intputRegister.length)

  val input: mutable.Queue[Long] = mutable.Queue.empty
  val processor = new IntCodeProcessor(register, input)
  val ship = Array.fill(100, 100)(' ')
  var position: (Int, Int) = (50, 50)
  var direction: (Int, Int) = (0, -1)

  def turnLeft(): Unit = direction = direction match {
    case (0, -1) => (-1, 0)
    case (-1, 0) => (0, 1)
    case (0, 1) => (1, 0)
    case (1, 0) => (0, -1)
  }

  def turnRight(): Unit = direction = direction match {
    case (0, -1) => (1, 0)
    case (1, 0) => (0, 1)
    case (0, 1) => (-1, 0)
    case (-1, 0) => (0, -1)
  }

  def moveForward(): Unit = {
    position = (position._1 + direction._1, position._2 + direction._2)
  }

  def done(): Unit = {
    val painted = ship.map(_.count(_ != ' ')).sum
    println(s"Painted $painted tiles")
    val transposed = ship.transpose
    transposed.foreach(line => println(line.map {
      case 'W' => '\u2588'
      case _ => ' '
    }.mkString))
    System.exit(0)
  }

  var output = 0L
  ship(position._1)(position._2) = 'W'
  while (output != Long.MinValue) {
    if (ship(position._1)(position._2) == 'W')
      input.enqueue(1L)
    else
      input.enqueue(0L)
    output = processor.run()
    output match {
      case 0L => ship(position._1)(position._2) = 'B'
      case 1L => ship(position._1)(position._2) = 'W'
      case Long.MinValue => done()
    }
    output = processor.run()
    output match {
      case 0L => turnLeft()
      case 1L => turnRight()
      case Long.MinValue => done()
    }
    moveForward()
  }
}
