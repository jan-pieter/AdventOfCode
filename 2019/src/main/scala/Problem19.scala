import scala.collection.mutable
import scala.io.Source

object Problem19 extends App {

  val intputRegister = Source.fromResource("19-input.txt").getLines().next().split(",").map(_.toLong)

//  val world = Array.fill(50, 50)(' ')
//
//  for {
//    x <- 0 until 50
//    y <- 0 until 50
//  } yield {
//    val register = Array.fill(1000000)(0L)
//    Array.copy(intputRegister, 0, register, 0, intputRegister.length)
//    val processor = new IntCodeProcessor(register, mutable.Queue(x, y))
//    val result = processor.run()
//    result match {
//      case 0L => world(y)(x) = '.'
//      case 1L => world(y)(x) = '#'
//    }
//  }
//
//  val answer = world.map(_.count(_ == '#')).sum
//
//  println(s"Answer: $answer")
//
//  world.foreach(line => println(line.mkString))

  var y = 937
  var x = 370
  var found = false
  var result = 0L
  var firstX = 0L

  while (!found || result == 1L) {
    val register = Array.fill(10000)(0L)
    Array.copy(intputRegister, 0, register, 0, intputRegister.length)
    val processor = new IntCodeProcessor(register, mutable.Queue(x, y))
    result = processor.run()
    if (result == 1L && !found){
      println(s"First # at $x")
      firstX = x
      found = true
    }
    x += 1
  }

  println(s"Last # at ${x-1}")
  println(s"Width ${x-1-firstX}")

  println(s"$firstX,$y: ${at(firstX.toInt,y)}")
  println(s"${firstX+99},$y: ${at(firstX.toInt+99,y)}")
  println(s"$firstX,${y+99}: ${at(firstX.toInt,y+99)}")
  println(s"${firstX+99},${y+99}: ${at(firstX.toInt+99,y+99)}")
  println("")
  println(s"${x-101},$y: ${at(x-101,y)}")
  println(s"${x-2},$y: ${at(x-2,y)}")
  println(s"${x-101},${y+99}: ${at(x-101,y+99)}")
  println(s"${x-2},${y+99}: ${at(x-2,y+99)}")

  def at(x: Int, y: Int): Long = {
    val register = Array.fill(10000)(0L)
    Array.copy(intputRegister, 0, register, 0, intputRegister.length)
    val processor = new IntCodeProcessor(register, mutable.Queue(x, y))
    processor.run()
  }


}
