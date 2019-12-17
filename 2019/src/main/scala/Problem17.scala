import scala.collection.mutable
import scala.io.Source

object Problem17 extends App {

  val intputRegister = Source.fromResource("17-input.txt").getLines().next().split(",").map(_.toLong)

  val register = Array.fill(1000000)(0L)
  Array.copy(intputRegister, 0, register, 0, intputRegister.length)

  val input = mutable.Queue[Long]()
  val processor = new IntCodeProcessor(register, input)

  val world = Array.fill(50,50)(' ')
  var x,y = 0

  var output = -1L
  while (output != Long.MinValue) {
    output = processor.run()
    output match {
      case Long.MinValue => //no-op
      case 10L =>
        x = 0
        y += 1
      case other =>
        world(y)(x) = other.toChar
        x += 1
    }
  }

  world.foreach(line => println(line.mkString))

  val result = (for {
    posX <- world(0).indices if posX > 0 && posX < world(0).length - 2
    posY <- world.indices if posY > 0 && posY < world.length
  } yield {
    if (world(posY)(posX) == '#' && world(posY-1)(posX) == '#' && world(posY+1)(posX) == '#' && world(posY)(posX-1) == '#' && world(posY)(posX+1) == '#')
      posX * posY
    else
      0
  }).sum

  println(s"Alignmentparameter sum: $result")


}
