import scala.collection.mutable
import scala.io.Source
import scala.util.Random

object Problem15 extends App {

  val intputRegister = Source.fromResource("15-input.txt").getLines().next().split(",").map(_.toLong)

  val register = Array.fill(1000000)(0L)
  Array.copy(intputRegister, 0, register, 0, intputRegister.length)
  val queue = mutable.Queue[Long]()

  val processor = new IntCodeProcessor(register, queue)

  var position = (50, 50) // Y, X
  var output = -1L
  val world: Array[Array[Char]] = Array.fill(100, 100)(' ')
  world(50)(50) = '.'
  var stepCount = 0L

  var steps: List[(Int, Int)] = Nil
  var done = false

  while (!done && stepCount <= 100000) {
    def chooseNext(): (Int, (Int, Int)) = {
      List(1, 2, 3, 4).find { input =>
        val nextPosition = input match {
          case 1 => (position._1 - 1, position._2)
          case 2 => (position._1 + 1, position._2)
          case 3 => (position._1, position._2 - 1)
          case 4 => (position._1, position._2 + 1)
        }
        world(nextPosition._1)(nextPosition._2) == ' '
      } match {
        case None => steps match {
          case Nil =>
            done = true
            (-1, position)
          case head :: _ =>
            head match {
              case (y, x) if y == position._1 - 1 && x == position._2 => (1, head)
              case (y, x) if y == position._1 + 1 && x == position._2 => (2, head)
              case (y, x) if y == position._1 && x == position._2 - 1 => (3, head)
              case (y, x) if y == position._1 && x == position._2 + 1 => (4, head)
              case (y, x) =>
                println(s"Match error $y $x $position")
                world(position._1)(position._2) = 'D'
                world.foreach(line => println(line.mkString))
                System.exit(1)
                (-1, position)
            }
        }
        case Some(input) =>
          val nextPosition = input match {
            case 1 => (position._1 - 1, position._2)
            case 2 => (position._1 + 1, position._2)
            case 3 => (position._1, position._2 - 1)
            case 4 => (position._1, position._2 + 1)
          }
          (input, nextPosition)
      }
    }
    val (input, nextPosition) = chooseNext()
//    println(nextPosition)
    if (!done) {
      queue.enqueue(input)
      output = processor.run()
      output match {
        case 0 =>
          world(nextPosition._1)(nextPosition._2) = 'W'
        case 1 =>
          world(nextPosition._1)(nextPosition._2) = '.'
          if (steps.headOption.exists(prevPos => prevPos._1 == nextPosition._1 && prevPos._2 == nextPosition._2)) {
            steps = steps.tail
          } else {
            steps = position :: steps
          }
          position = nextPosition
        case 2 =>
          world(nextPosition._1)(nextPosition._2) = 'O'
          if (steps.headOption.exists(prevPos => prevPos._1 == nextPosition._1 && prevPos._2 == nextPosition._2)) {
            steps = steps.tail
          } else {
            steps = position :: steps
          }
          println(s"Found O after ${steps.size} steps")
          position = nextPosition
      }
    }
    stepCount += 1L
//    if (stepCount % 10000 == 0) {
//      world.foreach(line => println(line.mkString))
//    }
  }
//  world(50)(50) = 'S'
  world.foreach(line => println(line.mkString))

  var minutes: Int = 0
  var oxygenToAdd: mutable.Set[(Int, Int)] = mutable.Set.empty
  def addOxygen(y: Int, x: Int): Unit = {
    if (world(y)(x) == '.')
      world(y)(x) = 'O'
  }
  while (world.map(_.count(_ == '.')).sum > 0) {
    for {
      y <- 0 until 100
      x <- 0 until 100
    } yield {
      if (world(y)(x) == 'O') {
        oxygenToAdd.add((y+1, x))
        oxygenToAdd.add((y-1, x))
        oxygenToAdd.add((y, x+1))
        oxygenToAdd.add((y, x-1))
      }
    }
    minutes = minutes + 1
    oxygenToAdd.foreach(pos => addOxygen(pos._1, pos._2))
    oxygenToAdd = mutable.Set.empty
  }
  world.foreach(line => println(line.mkString))

  println(s"Oxygen everywhere after $minutes minutes")
}

